{
  description = "K Kore Language Haskell Backend";
  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    z3-src = {
      url = "github:Z3Prover/z3/z3-4.8.15";
      flake = false;
    };
    nixpkgs22_05.url = "nixpkgs/nixos-22.05";
  };
  outputs = { self, nixpkgs, nixpkgs22_05, haskell-nix, z3-src }:
    let
      z3-overlay = (final: prev: {
        z3 = prev.z3.overrideAttrs (old: {
          src = z3-src;
          version =
            # hacky way of keeping the version number of this derivation consistent with
            # the z3-src input.
            let lock = builtins.fromJSON (builtins.readFile ./flake.lock);
            in builtins.replaceStrings [ "z3-" ] [ "" ]
            lock.nodes.z3-src.original.ref;
        });
      });
      perSystem = nixpkgs.lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      nixpkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay ];
          inherit (haskell-nix) config;
        };
      nixpkgs22_05For = system:
        import nixpkgs22_05 {
          inherit system;
          overlays = [ z3-overlay ];
        };

      haskell-backend-src = { pkgs, ghc, postPatch ? "" }:
        pkgs.applyPatches {
          name = "haskell-backend-src";
          # make sure we remove all nix files and flake.lock, since any changes to these triggers re-compilation of kore
          src = pkgs.nix-gitignore.gitignoreSourcePure [
            "./github"
            "/nix"
            "*.nix"
            "*.nix.sh"
            "/.github"
            "flake.*"
            "/profile"
            "/profile-*"
            ./.gitignore
          ] ./.;
          postPatch = ''
            substituteInPlace kore/src/Kore/VersionInfo.hs \
              --replace '$(GitRev.gitHash)' '"${self.rev or "dirty"}-${ghc}"'
            ${postPatch}
          '';
        };

      projectOverlay = { pkgs, pkgs', src, shell ? { }
        , compiler-nix-name ? "ghc8107", profiling ? false
        , profilingDetail ? "toplevel-functions", ghcOptions ? [ ] }:
        let
          add-z3 = exe: {
            build-tools = with pkgs'; lib.mkForce [ makeWrapper ];
            postInstall = ''
              wrapProgram $out/bin/${exe} --prefix PATH : ${
                with pkgs';
                lib.makeBinPath [ z3 ]
              }
            '';
          };
          self = pkgs.haskell-nix.stackProject' ({
            inherit shell src compiler-nix-name;
            modules = [{
              enableLibraryProfiling = profiling;
              packages.kore = {
                enableLibraryProfiling = profiling;
                enableProfiling = profiling;
                inherit profilingDetail ghcOptions;

                # Wrap all the kore-* executables which use Z3 with
                # the path to our pinned Z3 version
                components.exes.kore-exec = add-z3 "kore-exec";
                components.exes.kore-rpc = add-z3 "kore-rpc";
                components.exes.kore-repl = add-z3 "kore-repl";
                components.exes.kore-check-functions =
                  add-z3 "kore-check-functions";
              };
            }];
            materialized = ./nix + "/kore-${compiler-nix-name}.nix.d";
          });
        in self // {
          rematerialize-kore = pkgs.writeShellScriptBin
            "rematerialize-kore-nix-${compiler-nix-name}" ''
              #!/bin/sh
              ${self.stack-nix.passthru.generateMaterialized} ./nix/kore-${compiler-nix-name}.nix.d
              ${pkgs.nixfmt}/bin/nixfmt ./nix/kore-${compiler-nix-name}.nix.d/*.nix
            '';
        };

      projectForGhc = { ghc, profiling ? false
        , profilingDetail ? "toplevel-functions", ghcOptions ? [ ] }:
        perSystem (system:
          let
            pkgs = nixpkgsFor system;
            pkgs' = nixpkgs22_05For system;
          in projectOverlay {
            inherit pkgs pkgs' profiling ghcOptions;
            compiler-nix-name = ghc;

            src = haskell-backend-src {
              inherit ghc;
              pkgs = pkgs';
            };

            shell = {
              # We use the ones from Nixpkgs, since they are cached reliably.
              # Eventually we will probably want to build these with haskell.nix.
              nativeBuildInputs = [
                pkgs'.cabal-install
                pkgs'.hlint
                pkgs'.haskellPackages.fourmolu
                pkgs'.stack
                pkgs'.nixfmt
                pkgs'.haskellPackages.eventlog2html
                pkgs'.haskellPackages.ghc-prof-flamegraph
                pkgs'.haskellPackages.hs-speedscope
                pkgs'.zlib
              ] ++ (if system == "aarch64-darwin" then
                [ pkgs'.llvm_12 ]
              else
                [ ]);
            };
          });
    in {
      prelude-kore = ./src/main/kore/prelude.kore;

      project = projectForGhc { ghc = "ghc925"; };

      projectProfilingEventlog = projectForGhc {
        ghc = "ghc925";
        profiling = true;
        ghcOptions = [ "-eventlog" ];
      };


      projectEventlogInfoTable = projectForGhc {
        ghc = "ghc925";
        ghcOptions =
          [ "-finfo-table-map" "-fdistinct-constructor-tables" "-eventlog" ];
      };

      flake = perSystem (system: self.project.${system}.flake { });
      
      flakeGhc9 = perSystem (system: self.projectGhc9.${system}.flake { });

      packages = perSystem (system:
        self.flake.${system}.packages // {
          update-cabal = self.project.${system}.rematerialize-kore;
          update-cabal-ghc9 = self.projectGhc9.${system}.rematerialize-kore;
          kore-exec =
            self.project.${system}.hsPkgs.kore.components.exes.kore-exec;
          kore-exec-prof =
            self.projectProfilingEventlog.${system}.hsPkgs.kore.components.exes.kore-exec;
          kore-exec-infotable =
            self.projectEventlogInfoTable.${system}.hsPkgs.kore.components.exes.kore-exec;
        });

      apps = perSystem (system:
        self.flake.${system}.apps // (let
          pkgs = nixpkgs22_05For system;
          profiling-script = pkgs.callPackage ./nix/run-profiling.nix {
            inherit (pkgs.haskellPackages)
              hp2pretty hs-speedscope eventlog2html;
            kore-exec =
              self.project.${system}.hsPkgs.kore.components.exes.kore-exec;
            kore-exec-prof =
              self.projectProfilingEventlog.${system}.hsPkgs.kore.components.exes.kore-exec;
            kore-exec-infotable =
              self.projectEventlogInfoTable.${system}.hsPkgs.kore.components.exes.kore-exec;
          };
          scripts = pkgs.symlinkJoin {
            name = "fourmolu-format";
            paths = [ ./scripts ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/fourmolu.sh \
                --set PATH ${
                  with pkgs;
                  lib.makeBinPath [ haskellPackages.fourmolu fd findutils ]
                }
            '';
          };
        in {
          profile = {
            type = "app";
            program = "${profiling-script}/bin/run-profiling";
          };
          format = {
            type = "app";
            program = "${scripts}/fourmolu.sh";
          };
          remove-import-groups = {
            type = "app";
            program = "${scripts}/fourmolu.sh";
          };
        }));

      devShells = perSystem (system: {
        default = self.flake.${system}.devShell;
      });
      devShell = perSystem (system: self.devShells.${system}.default);

      overlay = nixpkgs.lib.composeManyExtensions [
        haskell-nix.overlay

        (final: prev: {
          haskell-backend-stackProject = projectOverlay {
            pkgs = prev;
            pkgs' = final;
            src = haskell-backend-src {
              pkgs = prev;
              ghc = "ghc925";
            };
          };
        })

        z3-overlay
      ];
    };
}
