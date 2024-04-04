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
      ghcVersion = pkgs: pkgs.haskell.packages.ghc964;
    in {
      overlay = final: prev: {
        haskell-backend = final.stacklock2nix {
          stackYaml = ./stack.yaml;
          # This should based on the compiler version from the resolver in stack.yaml.
          baseHaskellPkgSet = ghcVersion final;
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

              # everything belwo should be removed once HLS in nixpkgs catches up to ghc 9.6
              doctest-discover = dontCheck hprev.doctest-discover;
              extensions = doJailbreak (dontCheck hprev.extensions);
              filtrable = doJailbreak (dontCheck hprev.filtrable);
              hie-bios = dontCheck hprev.hie-bios;
              hw-fingertree = dontCheck hprev.hw-fingertree;
              generic-arbitrary = dontCheck hprev.generic-arbitrary;
              fourmolu = dontCheck hprev.fourmolu;
              list-t = dontCheck hprev.list-t;
              ghcide = (doJailbreak (overrideCabal hprev.ghcide (drv: {
                version = "2.6.0.0";
                sha256 = "sha256-RSWtZE5ikoVW656l13Il6rgGRaW1+UdtIYxu6RhtKfU=";
              }))).override {
                implicit-hie-cradle = null;
              };
              hiedb = overrideCabal hprev.hiedb (drv: {
                version = "0.6.0.0";
                sha256 = "sha256-wpSO71esKsl4BddH5tPaqlIbPISonMv4fYEFlDwj4Ps=";
              });
              implicit-hie = overrideCabal hprev.implicit-hie (drv: {
                version = "0.1.4.0";
                sha256 = "sha256-kxgU1sG7n49tVxYXg+rLe5XmY5jhsg1lLsoHWSBt7yE=";
              });
              stan = overrideCabal hprev.stan (drv: {
                version = "0.1.2.0";
                sha256 = "sha256-+ntlOlKmLY3HjR8MA+Kzssre2DaCAeqgDIDDZ6VPopE=";
              });
              haskell-language-server = dontCheck (overrideCabal hprev.haskell-language-server (drv: {
                version = "2.6.0.0";
                sha256 = "sha256-NlW410Fhb44y0BgSdPgKSNm43KR9P9ImdcrfoByoDkg=";
                libraryHaskellDepends = with hfinal; drv.libraryHaskellDepends ++ [hls-semantic-tokens-plugin hls-floskell-plugin ];
              }));
              ansi-wl-pprint = hfinal.callPackage
                ({ mkDerivation, base, lib, prettyprinter-compat-ansi-wl-pprint }:
                mkDerivation {
                  pname = "ansi-wl-pprint";
                  version = "1.0.2";
                  sha256 = "234e1813a178e5466d121635e190e6ff33ea6f09c45120138824d5de76af2747";
                  isLibrary = true;
                  isExecutable = true;
                  libraryHaskellDepends = [
                    base prettyprinter-compat-ansi-wl-pprint
                  ];
                  homepage = "http://github.com/ekmett/ansi-wl-pprint";
                  description = "The Wadler/Leijen Pretty Printer for colored ANSI terminal output";
                  license = lib.licenses.bsd3;
                }) { };
              floskell = overrideCabal hprev.floskell (drv: {
                version = "0.11.1";
                sha256 = "sha256-drMnL3rOV+hJjyLWDI7BLanOkZFcl9YGFl+7Zuj2qb0=";
                libraryHaskellDepends = with hfinal; drv.libraryHaskellDepends ++ [ansi-wl-pprint];
              });
              hls-floskell-plugin = dontCheck (hfinal.callPackage
                ({ mkDerivation, base, filepath, floskell, ghcide, hls-plugin-api
                , hls-test-utils, lib, lsp-types, mtl, text, transformers
                }:
                mkDerivation {
                  pname = "hls-floskell-plugin";
                  version = "2.6.0.0";
                  sha256 = "33c6239e42774f3bc37ab0daf0f0dc5130a101bca91e8963fe1032eb2942d3f3";
                  libraryHaskellDepends = [
                    base floskell ghcide hls-plugin-api lsp-types mtl text transformers
                  ];
                  testHaskellDepends = [ base filepath hls-test-utils ];
                  description = "Integration with the Floskell code formatter";
                  license = lib.licenses.asl20;
                }) { });
              hls-semantic-tokens-plugin = dontCheck (hfinal.callPackage
                ({ mkDerivation, aeson, array, base, bytestring, containers
                , data-default, deepseq, extra, filepath, ghc, ghcide
                , ghcide-test-utils, hiedb, hls-graph, hls-plugin-api
                , hls-test-utils, lens, lib, lsp, lsp-test, mtl, sqlite-simple, syb
                , template-haskell, text, text-rope, transformers
                , unordered-containers
                }:
                mkDerivation {
                  pname = "hls-semantic-tokens-plugin";
                  version = "2.6.0.0";
                  sha256 = "0502aa19f5406102ddf8e8a37bf94f3cde0ce02f2c687967ef1f3319bcd98faa";
                  libraryHaskellDepends = [
                    aeson array base bytestring containers data-default deepseq extra
                    ghcide hiedb hls-graph hls-plugin-api lens lsp mtl sqlite-simple
                    syb template-haskell text transformers unordered-containers
                  ];
                  testHaskellDepends = [
                    aeson base bytestring containers data-default extra filepath ghc
                    ghcide ghcide-test-utils hls-plugin-api hls-test-utils lens lsp
                    lsp-test template-haskell text text-rope
                  ];
                  description = "Call hierarchy plugin for Haskell Language Server";
                  license = lib.licenses.asl20;
                }) { });
            } // prev.lib.attrsets.mapAttrs ( pkg: sha256: dontCheck (overrideCabal hprev.${pkg} (drv: {
                version = "2.6.0.0";
                inherit sha256;
              })))  {
                hls-cabal-plugin = "sha256-thKRbE9J3U6gsk4K6HeXz/p+cFJMaaRqIYgCPHeamQM=";
                hls-call-hierarchy-plugin = "sha256-gTHZol6/7R9E98jtIQFDF/IHkW8mz5Uoop3mHvNMi4k=";
                hls-plugin-api = "sha256-xE1+eOv0DHFvC5J8EgLFMaBjEDhim5YqRPx/B6k3JII=";
                hls-graph = "sha256-fHbVIU/zjhYU/zixs7ZUYDmEfIGybkJXsyoDodSSlGY=";
                hls-cabal-fmt-plugin = "sha256-a35GkfYX5w9lGR7N8PLWB7lW+vO2fB4sgvjtnUEvE/o=";
                hls-change-type-signature-plugin = "sha256-IE/jxyD1n7AxbCYpawogVMlrKbA4tx+wXcMbcQ65eeE=";
                hls-class-plugin = "sha256-nYbVMn2Wv4HNq30tyxa7nPuxmpY5Rf04WmU5U4QXK0o=";
                hls-code-range-plugin = "sha256-RTspLpA+Sl9AFuP46hQvUcUD2VM82XxsK9TtPhTiJMc=";
                hls-eval-plugin = "sha256-6eym4FVkfSgAK+pkEYstpnPc74uxRtns+1pSpaO+24k=";
                hls-explicit-fixity-plugin = "sha256-StkthuISYz7P4LUgQw2I4Xk7VR5RtwZkNHX73p8FqQI=";
                hls-explicit-imports-plugin = "sha256-bMyB0NpSnRF9/qpU1iGK/2xrhorfVK4j1A0X+UDBZOg=";
                hls-explicit-record-fields-plugin = "sha256-hPQTlp3P1lyZFJmtbloPP69ax5+ZoGHlW0KijTLoQ9o=";
                hls-fourmolu-plugin = "sha256-ln8B2d+h6Y5BEZfx15AHXAqRM/Ok3EzE6aSrfVwl9bw=";
                hls-module-name-plugin = "sha256-fwPizMINMuha9Ql5pauMeYtBm90EmKILw01Z05g8ytk=";
                hls-ormolu-plugin = "sha256-osfRilT2ujMNbl5RglzUEyCWC1XslRqZkuK+oXVC1zI=";
                hls-pragmas-plugin = "sha256-YyXrbN6bgWKn2keix8EXXE8v6ndXV9YZPRm0KmaanwE=";
                hls-qualify-imported-names-plugin = "sha256-pgry1GCRkVGblrsuW+jNy7Q0F0qhEkCGmBwI4FSHTyI=";
                hls-refactor-plugin = "sha256-Tn6nVZ8QwfH0sDtTC/M4zyysKSPWoJ/0EwNZvAc7HlM=";
                hls-stylish-haskell-plugin = "sha256-45wRcUVj1ky5+OL+6RDOuQILmCTp7KJJmE2fVSsyIJw=";
                hls-hlint-plugin = "sha256-jPvdHXQVfMD57Brp+RIdT1Rn6ZGIj2Yg93Fgiy5PqEc=";
                hls-alternate-number-format-plugin = "sha256-LHx8svXYP0tO4xsq5ismnJY3PoSuJuUIC9AUx1sfgtg=";
                hls-overloaded-record-dot-plugin = "sha256-0Jl+ZHQLPzs7R9iAagAp70s0zeK6FLNMUc/K3KJ2i24=";
                hls-rename-plugin = "sha256-j2TfjRmQEVXbLfbDXcTuljzlEhlYlPvD94MwlvKB8pU=";
                hls-retrie-plugin = "sha256-8I/duDfMfmwfRXjYx+ItaYJTws2qMSaMHod2mQ4mlIU=";
                hls-splice-plugin = "sha256-En+UkYhmdB45gtlJJgTz2MbCqfgKrC2OAVVV23pG3QE=";
                hls-stan-plugin = "sha256-LQL6ildOWteT81f4S3ZAZQ1kMjTOQbXDz5qx4rQXHP4=";
                hls-gadt-plugin = "sha256-Lz/yoNqIvcbja3IBoTY5xGw+a49RXDsEZEMTAaoARik=";
                hls-test-utils = "sha256-mUeg417Paw3UFlSUhHHLWmr6mJG2mT6ilXTmlU1WAlg=";
              }  ;

          # Additional packages that should be available for development.
          additionalDevShellNativeBuildInputs = stacklockHaskellPkgSet:
            with ghcVersion final; [
              cabal-install
              fourmolu
              hlint
              stacklockHaskellPkgSet.haskell-language-server
              final.haskell-language-server
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
          inherit (pkgs.haskell-backend.pkgSet) haskell-language-server;
        });

      devShells = perSystem (system: {
        # Separate fourmolu and cabal shells just for CI
        fourmolu = with nixpkgsCleanFor system;
          mkShell {
            nativeBuildInputs = [
              (haskell.lib.justStaticExecutables
               (ghcVersion pkgs).fourmolu)
              (haskell.lib.justStaticExecutables
               (ghcVersion pkgs).hlint)
            ];
          };
        cabal = let pkgs = nixpkgsFor system;
        in pkgs.haskell-backend.pkgSet.shellFor {
          packages = pkgs.haskell-backend.localPkgsSelector;
          nativeBuildInputs =
            [ (ghcVersion pkgs).cabal-install pkgs.z3 ];
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
