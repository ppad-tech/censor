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
      url = "git+file:///Users/jtobin/src/ppad/fixed";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
    };
    flake-utils.follows = "ppad-nixpkgs/flake-utils";
    nixpkgs.follows = "ppad-nixpkgs/nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, ppad-nixpkgs
            , ppad-eproc, ppad-fixed }:
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

        hpkgs = pkgs.haskell.packages.ghc910.extend (new: old: {
          ppad-eproc = eproc;
          ppad-fixed = fixed-llvm;
          ${lib} = hlib.enableCabalFlag
            (new.callCabal2nixWithOptions lib ./. "--flag=demo" {
               ppad-eproc = new.ppad-eproc;
               ppad-fixed = new.ppad-fixed;
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
