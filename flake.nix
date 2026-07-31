{
  description = "Anytime-valid sequential constant-time testing.";

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

        poly1305 = ppad-poly1305.packages.${system}.default;
        poly1305-llvm =
          hlib.addBuildTools
            (hlib.enableCabalFlag poly1305 "llvm")
            [ llvm clang ];

        # rust staticlib exercised by censor-demo-ffi. bundles all
        # FFI targets (the 32-byte inequality witnesses and the
        # fastcrypto::secp256r1 sign bridge) in one crate so the
        # Haskell linker sees only one copy of the Rust std runtime
        # symbols -- ld.gold rejects the duplicates that ld on
        # macOS silently accepts when two staticlibs are linked.
        #
        # outputHashes pin the git-sourced crates (fastcrypto and
        # fastcrypto-derive, both from MystenLabs/fastcrypto at the
        # same rev so they share the same hash).
        rs-demo = pkgs.stdenv.mkDerivation {
          pname   = "censor-rs-demo";
          version = "0.1.0";
          src     = ./rs-demo;
          cargoDeps = pkgs.rustPlatform.importCargoLock {
            lockFile = ./rs-demo/Cargo.lock;
            outputHashes = {
              "fastcrypto-0.1.9" =
                "sha256-pku0vKWI5sIPcs0ef9fBfLHvZkI5DUhmmVLxkGRy9SQ=";
              "fastcrypto-derive-0.1.3" =
                "sha256-pku0vKWI5sIPcs0ef9fBfLHvZkI5DUhmmVLxkGRy9SQ=";
            };
          };
          nativeBuildInputs = [
            pkgs.cargo
            pkgs.rustc
            pkgs.rustPlatform.cargoSetupHook
          ];
          buildPhase = ''
            runHook preBuild
            HOME=$TMPDIR cargo build --release --offline \
              --target-dir target
            runHook postBuild
          '';
          installPhase = ''
            runHook preInstall
            mkdir -p $out/lib
            cp target/release/libcensor_rs_demo.a $out/lib/
            runHook postInstall
          '';
        };

        hpkgs = pkgs.haskell.packages.ghc910.extend (new: old: {
          ppad-eproc = eproc;
          ppad-fixed = fixed-llvm;
          ppad-sha256 = sha256-llvm;
          ppad-sha512 = sha512-llvm;
          ppad-secp256k1 = secp-llvm;
          ppad-bip32 = bip32-llvm;
          ppad-aead = aead-llvm;
          ppad-poly1305 = poly1305-llvm;
          ${lib} = hlib.overrideCabal (hlib.enableCabalFlag
            (hlib.enableCabalFlag
              (hlib.enableCabalFlag
                (new.callCabal2nixWithOptions lib ./.
                   "--flag=demo --flag=demo-ffi --flag=poly" {
                   ppad-eproc = new.ppad-eproc;
                   ppad-fixed = new.ppad-fixed;
                   ppad-sha256 = new.ppad-sha256;
                   ppad-sha512 = new.ppad-sha512;
                   ppad-secp256k1 = new.ppad-secp256k1;
                   ppad-bip32 = new.ppad-bip32;
                   ppad-aead = new.ppad-aead;
                   ppad-poly1305 = new.ppad-poly1305;
                   # supplied at link time via --extra-lib-dirs
                   # below; cabal2nix would otherwise treat the
                   # extra-libraries entry as a missing nix arg.
                   censor_rs_demo = null;
                 })
                "demo")
              "demo-ffi")
            "poly")
            (drv: {
              configureFlags = (drv.configureFlags or []) ++ [
                "--extra-lib-dirs=${rs-demo}/lib"
              ];
            });
        });

        cc    = pkgs.stdenv.cc;
        ghc   = hpkgs.ghc;
        cabal = hpkgs.cabal-install;
        cargo = pkgs.cargo;
        rustc = pkgs.rustc;
      in
        {
          packages.default = hpkgs.${lib};

          packages.haddock = hpkgs.${lib}.doc;

          # 'nix flake check' realises the full package (demo,
          # demo-ffi, poly flags on), catching breaks that a plain
          # 'cabal build' skips because those flags default to off.
          checks.default = hpkgs.${lib};

          devShells.default = hpkgs.shellFor {
            packages = p: [
              p.${lib}
            ];

            buildInputs = [
              cabal
              cc
              clang
              llvm
              cargo
              rustc
            ];

            shellHook = ''
              PS1="[${lib}] \w$ "
              echo "entering ${system} shell, using"
              echo "cc:    $(${cc}/bin/cc --version)"
              echo "clang: $(${clang}/bin/clang --version | head -1)"
              echo "ghc:   $(${ghc}/bin/ghc --version)"
              echo "cabal: $(${cabal}/bin/cabal --version)"
              echo "llc:   $(${llvm}/bin/llc --version | head -2 | tail -1)"
              echo "rustc: $(${rustc}/bin/rustc --version)"
            '';
          };
        }
      );
}
