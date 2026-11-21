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
    flake-utils.follows = "ppad-nixpkgs/flake-utils";
    nixpkgs.follows = "ppad-nixpkgs/nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, ppad-nixpkgs, ppad-eproc }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        lib = "ppad-censor";

        pkgs  = import nixpkgs { inherit system; };
        llvm = pkgs.llvmPackages_19.llvm;
        clang = pkgs.llvmPackages_19.clang;

        eproc = ppad-eproc.packages.${system}.default;

        hpkgs = pkgs.haskell.packages.ghc910.extend (new: old: {
          ppad-eproc = eproc;
          ${lib} = new.callCabal2nix lib ./. {
            ppad-eproc = new.ppad-eproc;
          };
        });

        cc    = pkgs.stdenv.cc;
        ghc   = hpkgs.ghc;
        cabal = hpkgs.cabal-install;
      in
        {
          packages.default = hpkgs.${lib};

          packages.haddock = hpkgs.${lib}.doc;

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
            ];

            shellHook = ''
              PS1="[${lib}] \w$ "
              echo "entering ${system} shell, using"
              echo "cc:    $(${cc}/bin/cc --version)"
              echo "clang: $(${clang}/bin/clang --version | head -1)"
              echo "ghc:   $(${ghc}/bin/ghc --version)"
              echo "cabal: $(${cabal}/bin/cabal --version)"
              echo "llc:   $(${llvm}/bin/llc --version | head -2 | tail -1)"
            '';
          };
        }
      );
}
