{
  description = "Sequential constant-time testing for Haskell.";

  inputs = {
    ppad-nixpkgs = {
      type = "git";
      url  = "git://git.ppad.tech/nixpkgs.git";
      ref  = "master";
    };
    ppad-eproc = {
      type = "git";
      url  = "git://git.ppad.tech/eproc.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
    };
    ppad-fixed = {
      type = "git";
      url  = "git://git.ppad.tech/fixed.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
    };
    ppad-sha256 = {
      type = "git";
      url  = "git://git.ppad.tech/sha256.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
    };
    ppad-sha512 = {
      type = "git";
      url  = "git://git.ppad.tech/sha512.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
    };
    ppad-secp256k1 = {
      type = "git";
      url  = "git://git.ppad.tech/secp256k1.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
      # align secp256k1's hash deps with our top-level pins so
      # we don't end up with multiple versions in the graph.
      inputs.ppad-sha256.follows = "ppad-sha256";
      inputs.ppad-sha512.follows = "ppad-sha512";
    };
    ppad-bip32 = {
      type = "git";
      url  = "git://git.ppad.tech/bip32.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
      inputs.ppad-sha256.follows = "ppad-sha256";
      inputs.ppad-sha512.follows = "ppad-sha512";
      inputs.ppad-secp256k1.follows = "ppad-secp256k1";
      inputs.ppad-fixed.follows = "ppad-fixed";
    };
    # Hoisted so we can pin its ppad-fixed input to our top-level
    # ppad-fixed; without that, aead -> poly1305 brings its own
    # ppad-fixed and the build trips on multiple versions.
    ppad-poly1305 = {
      type = "git";
      url  = "git://git.ppad.tech/poly1305.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
      inputs.ppad-fixed.follows = "ppad-fixed";
    };
    ppad-aead = {
      type = "git";
      url  = "git://git.ppad.tech/aead.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
      inputs.ppad-poly1305.follows = "ppad-poly1305";
    };
    flake-utils.follows = "ppad-nixpkgs/flake-utils";
    nixpkgs.follows = "ppad-nixpkgs/nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, ppad-nixpkgs
            , ppad-eproc, ppad-fixed
            , ppad-sha256, ppad-sha512
            , ppad-secp256k1, ppad-bip32
            , ppad-poly1305, ppad-aead }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        lib = "ppad-censor";

        pkgs  = import nixpkgs { inherit system; };
        hlib  = pkgs.haskell.lib;
        llvm  = pkgs.llvmPackages_19.llvm;
        clang = pkgs.llvmPackages_19.clang;

        eproc = ppad-eproc.packages.${system}.default;

        fixed = ppad-fixed.packages.${system}.default;
        fixed-llvm =
          hlib.addBuildTools
            (hlib.enableCabalFlag fixed "llvm")
            [ llvm clang ];

        sha256 = ppad-sha256.packages.${system}.default;
        sha256-llvm =
          hlib.addBuildTools
            (hlib.enableCabalFlag sha256 "llvm")
            [ llvm clang ];

        sha512 = ppad-sha512.packages.${system}.default;
        sha512-llvm =
          hlib.addBuildTools
            (hlib.enableCabalFlag sha512 "llvm")
            [ llvm clang ];

        secp = ppad-secp256k1.packages.${system}.default;
        secp-llvm =
          hlib.addBuildTools
            (hlib.enableCabalFlag secp "llvm")
            [ llvm clang ];

        bip32 = ppad-bip32.packages.${system}.default;
        bip32-llvm =
          hlib.addBuildTools
            (hlib.enableCabalFlag bip32 "llvm")
            [ llvm clang ];

        aead = ppad-aead.packages.${system}.default;
        aead-llvm =
          hlib.addBuildTools
            (hlib.enableCabalFlag aead "llvm")
            [ llvm clang ];

        hpkgs = pkgs.haskell.packages.ghc910.extend (new: old: {
          ppad-eproc = eproc;
          ppad-fixed = fixed-llvm;
          ppad-sha256 = sha256-llvm;
          ppad-sha512 = sha512-llvm;
          ppad-secp256k1 = secp-llvm;
          ppad-bip32 = bip32-llvm;
          ppad-aead = aead-llvm;
          ${lib} = hlib.enableCabalFlag
            (new.callCabal2nixWithOptions lib ./. "--flag=demo" {
               ppad-eproc = new.ppad-eproc;
               ppad-fixed = new.ppad-fixed;
               ppad-sha256 = new.ppad-sha256;
               ppad-sha512 = new.ppad-sha512;
               ppad-secp256k1 = new.ppad-secp256k1;
               ppad-bip32 = new.ppad-bip32;
               ppad-aead = new.ppad-aead;
             })
            "demo";
        });

        cc    = pkgs.stdenv.cc;
        ghc   = hpkgs.ghc;
        cabal = hpkgs.cabal-install;
      in
        {
          packages.default = hpkgs.${lib};

          packages.haddock = hpkgs.${lib}.doc;

          devShells.default = hpkgs.shellFor {
            packages = p: [
              p.${lib}
            ];

            buildInputs = [
              cabal
              cc
              llvm
            ];

            shellHook = ''
              PS1="[${lib}] \w$ "
              echo "entering ${system} shell, using"
              echo "cc:    $(${cc}/bin/cc --version)"
              echo "ghc:   $(${ghc}/bin/ghc --version)"
              echo "cabal: $(${cabal}/bin/cabal --version)"
              echo "llc:   $(${llvm}/bin/llc --version | head -2 | tail -1)"
            '';
          };
        }
      );
}
