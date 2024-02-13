{
  description = "K Kore Language Haskell Backend";
  inputs = {
    rv-utils.url = "github:runtimeverification/rv-nix-tools";
    nixpkgs.follows = "rv-utils/nixpkgs";

    stacklock2nix.url = "github:cdepillabout/stacklock2nix";
    z3 = {
      url = "github:Z3Prover/z3/z3-4.12.1";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, stacklock2nix, z3, rv-utils }:
    let
      perSystem = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
      nixpkgsCleanFor = system: import nixpkgs { inherit system; };
      nixpkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ stacklock2nix.overlay self.overlay self.overlays.z3 ];
        };
      withZ3 = pkgs: pkg: exe:
        pkgs.stdenv.mkDerivation {
          name = exe;
          phases = [ "installPhase" ];
          buildInputs = with pkgs; [ makeWrapper ];
          installPhase = ''
            mkdir -p $out/bin
            makeWrapper ${pkg}/bin/${exe} $out/bin/${exe} --prefix PATH : ${pkgs.z3}/bin
          '';
        };
    in {
      overlay = final: prev: {
        haskell-backend = final.stacklock2nix {
          stackYaml = ./stack.yaml;
          # This should based on the compiler version from the resolver in stack.yaml.
          baseHaskellPkgSet = final.haskell.packages.ghc964;
          cabal2nixArgsOverrides = args:
            args // {
              # The Haskell package `"graphviz"` depends on the _system_
              # package `graphviz`, and takes the system package `graphviz` as one of its build
              # inputs, but it is actually getting passed _itself_ (not the system package
              # `graphviz`), which causes the infinite recursion.
              "graphviz" = _: { graphviz = final.graphviz; };
            };
          additionalHaskellPkgSetOverrides = hfinal: hprev:
            with final.haskell.lib; {
              decision-diagrams = dontCheck hprev.decision-diagrams;
              fgl = dontCheck hprev.fgl;
              fgl-arbitrary = dontCheck hprev.fgl-arbitrary;
              # haskeline = dontCheck hprev.haskeline;
              json-rpc = dontCheck hprev.json-rpc;
              kore = (overrideCabal hprev.kore (drv: {
                doCheck = false;
                postPatch = ''
                  ${drv.postPatch or ""}
                  substituteInPlace src/Kore/VersionInfo.hs \
                    --replace '$(GitRev.gitHash)' '"${self.rev or "dirty"}"'
                '';
                postInstall = '' 
                  ${drv.postInstall or ""}
                  rm $out/bin/kore-check-functions
                  rm $out/bin/kore-format
                '';
              })).override {
                # bit pathological, but ghc-compact is already included with the ghc compiler
                # and depending on another copy of ghc-compact breaks HLS in the dev shell.
                ghc-compact = null;
              };
              lifted-base = dontCheck hprev.lifted-base;
              prettyprinter = dontCheck hprev.prettyprinter;
              tar = dontCheck hprev.tar;
              # typerep-map = doJailbreak hprev.typerep-map;
            };

          # Additional packages that should be available for development.
          additionalDevShellNativeBuildInputs = stacklockHaskellPkgSet:
            with final; [
              haskell.packages.ghc964.cabal-install
              haskell.packages.ghc964.fourmolu
              haskell.packages.ghc964.hlint

              # (haskell-language-server.override {
              #   supportedGhcVersions = [ "964" ];
              # })

              final.z3
              final.secp256k1
            ];
          all-cabal-hashes = final.fetchurl {
            url =
              "https://github.com/commercialhaskell/all-cabal-hashes/archive/80fe3174b98134e50d4541c9c2a3803601f6fbb7.tar.gz";
            sha256 = "sha256-b3E6JLu1tBpZnPXBJxNXfjkglY/du8k1l+WTS8Fetr4=";
          };
        };
      };

      prelude-kore = ./src/main/kore/prelude.kore;

      packages = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          kore = with pkgs;
            haskell.lib.justStaticExecutables haskell-backend.pkgSet.kore;
        in {
          kore-exec = withZ3 pkgs kore "kore-exec";
          kore-match-disjunction = withZ3 pkgs kore "kore-match-disjunction";
          kore-parser = withZ3 pkgs kore "kore-parser";
          kore-repl = withZ3 pkgs kore "kore-repl";
          kore-rpc = withZ3 pkgs kore "kore-rpc";
        });

      devShells = perSystem (system: {
        # Separate fourmolu and cabal shells just for CI
        fourmolu = with nixpkgsCleanFor system;
          mkShell {
            nativeBuildInputs = [
              (haskell.lib.justStaticExecutables
                haskell.packages.ghc964.fourmolu)
            ];
          };
        cabal = let pkgs = nixpkgsFor system;
        in pkgs.haskell-backend.pkgSet.shellFor {
          packages = pkgs.haskell-backend.localPkgsSelector;
          nativeBuildInputs =
            [ pkgs.haskell.packages.ghc964.cabal-install pkgs.z3 ];
        };
      });

      devShell =
        perSystem (system: (nixpkgsFor system).haskell-backend.devShell);

      overlays = {
        z3 = (final: prev: {
          z3 = prev.z3.overrideAttrs (_: {
            src = z3;
            version = let
              release = builtins.readFile "${z3}/scripts/release.yml";
              # Read the release version from scripts/release.yml
            in builtins.head
            (builtins.match ".+ReleaseVersion: '([^']+).+" release);
          });
        });
        integration-tests = (final: prev: {
          kore-tests = final.callPackage ./nix/integration-shell.nix {
            python = final.python3.withPackages (ps:
              with ps;
              [
                (buildPythonPackage rec {
                  pname = "jsonrpcclient";
                  version = "4.0.3";
                  src = prev.fetchFromGitHub {
                    owner = "explodinglabs";
                    repo = pname;
                    rev = version;
                    sha256 =
                      "sha256-xqQwqNFXatGzc4JprZY1OpdPPGgpP5/ucG/qyV/n8hw=";
                  };
                  doCheck = false;
                  format = "pyproject";
                  buildInputs = [ setuptools ];
                })
              ]);
          };
        });
      };
    };
}
