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
      # This should based on the compiler version from the resolver in stack.yaml.
      ghcVersion = pkgs: pkgs.haskell.packages.native-bignum.ghc965;
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
              crypton-x509 = dontCheck hprev.crypton-x509;
              cryptonite = disableCabalFlag hprev.cryptonite "integer-gmp";
              data-fix = doJailbreak hprev.data-fix;
              decision-diagrams = dontCheck hprev.decision-diagrams;
              fgl = dontCheck hprev.fgl;
              fgl-arbitrary = dontCheck hprev.fgl-arbitrary;
              graphviz = dontCheck hprev.graphviz;
              json-rpc = dontCheck hprev.json-rpc;
              lifted-base = dontCheck hprev.lifted-base;
              prettyprinter = dontCheck hprev.prettyprinter;
              semialign = doJailbreak hprev.semialign;
              smtlib-backends-process = dontCheck hprev.smtlib-backends-process;
              tar = dontCheck hprev.tar;
              text-short = doJailbreak hprev.text-short;
              these = doJailbreak hprev.these;
              ghc-prof = doJailbreak hprev.ghc-prof;
              hs-backend-booster = overrideCabal hprev.hs-backend-booster
                (drv: {
                  doCheck = false;
                  postPatch = ''
                    ${drv.postPatch or ""}
                    substituteInPlace library/Booster/VersionInfo.hs \
                      --replace '$(GitRev.gitHash)' '"${self.rev or "dirty"}"'
                  '';
                });
              kore = (overrideCabal hprev.kore (drv: {
                doCheck = false;
                postPatch = ''
                  ${drv.postPatch or ""}
                  substituteInPlace src/Kore/VersionInfo.hs \
                    --replace '$(GitRev.gitHash)' '"${self.rev or "dirty"}"'
                '';
                postInstall = ''
                  ${drv.postInstall or ""}
                '';
              })).override {
                # bit pathological, but ghc-compact is already included with the ghc compiler
                # and depending on another copy of ghc-compact breaks HLS in the dev shell.
                ghc-compact = null;
              };

            };

          # Additional packages that should be available for development.
          additionalDevShellNativeBuildInputs = stacklockHaskellPkgSet:
            with ghcVersion final; [
              cabal-install
              hpack
              fourmolu
              hlint
              final.haskell-language-server
              final.z3
              final.secp256k1
            ];
          # nix expects all inputs downloaded from the internet to have a hash,
          # so hackage is periodically downloaded, hashed and the hashes stored in a map.
          # this need to be bumped if changing the stack resolver
          all-cabal-hashes = final.fetchurl {
            url =
              "https://github.com/commercialhaskell/all-cabal-hashes/archive/8b0d7c881466c603de7975596cb203e3fd4890ad.tar.gz";
            sha256 = "sha256-jTNSwG37bHY5vLMo5yqtk93sTUzlsp/jeYCfXJGaivU=";
          };
        };
      };

      prelude-kore = ./src/main/kore/prelude.kore;

      packages = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          kore = with pkgs;
            haskell.lib.justStaticExecutables haskell-backend.pkgSet.kore;
          hs-backend-booster = with pkgs;
            haskell.lib.justStaticExecutables haskell-backend.pkgSet.hs-backend-booster;
          hs-backend-booster-dev-tools = with pkgs;
            haskell.lib.justStaticExecutables haskell-backend.pkgSet.hs-backend-booster-dev-tools;
        in {
          kore-exec = withZ3 pkgs kore "kore-exec";
          kore-match-disjunction = withZ3 pkgs hs-backend-booster-dev-tools "kore-match-disjunction";
          kore-parser = withZ3 pkgs hs-backend-booster-dev-tools "kore-parser";
          kore-repl = withZ3 pkgs kore "kore-repl";
          kore-rpc = withZ3 pkgs kore "kore-rpc";
          kore-rpc-booster = withZ3 pkgs hs-backend-booster "kore-rpc-booster";
          kore-rpc-client = withZ3 pkgs hs-backend-booster "kore-rpc-client";
          booster-dev = withZ3 pkgs hs-backend-booster-dev-tools "booster-dev";
          inherit (pkgs.haskell-backend.pkgSet) haskell-language-server;
        });

      devShells = perSystem (system: {
        # Separate fourmolu and cabal shells just for CI
        style = with nixpkgsCleanFor system;
          mkShell {
            nativeBuildInputs = [
              (haskell.lib.justStaticExecutables
               (ghcVersion pkgs).fourmolu)
              (haskell.lib.justStaticExecutables
               (ghcVersion pkgs).hlint)
              pkgs.hpack
            ];
            shellHook = ''
              hpack booster && hpack dev-tools
            '';
          };
        cabal = let pkgs = nixpkgsFor system;
        in pkgs.haskell-backend.pkgSet.shellFor {
          packages = pkgs.haskell-backend.localPkgsSelector;
          nativeBuildInputs = [
              (ghcVersion pkgs).cabal-install
              pkgs.hpack
              pkgs.jq
              pkgs.nix
              pkgs.z3
              pkgs.lsof
          ];
          shellHook = ''
            hpack booster && hpack dev-tools
          '';
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
