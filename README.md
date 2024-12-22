# Symbolic Execution Engine for the K Framework

## Structure of this project

This project implements the tools for symbolically executing programs in languages specified using the [K Framework]. The symbolic execution is performed at the level of `Kore` --- an intermediate representation produced by the K compiled from a language specification.

The K framework is a term rewriting-based specification language, and therefore its symbolic execution backend implements a symbolic term rewriter. Such a rewriter is implemented by the `kore-rpc-booster` executable --- a JSON RPC-based server that implements the [KORE RPC protocol].

The `kore-rpc` executable is a legacy version of the RPC-based rewriter that implements the same protocol. Finally, `kore-exec` and `kore-repl` are the deprecated non-RPC entry points.

Note that this project is low-level and is not intended to be a user-facing tool. For the Python-based theorem prover based on the KORE RPC protocol, see the [pyk] package in the [K Framework] repository.

### Kompiling a K definition and running the RPC server

The `kore-rpc-booster` binary takes a `kore` file definition, parses and internalises it and then launches an RPC server, which executes requests agains this definition. It additionally accepts a path to a dynamic library compiled by the LLVM backend, which is used for simplification of bool sorted terms. In order to build the kore definition and the shared library out of a K definition, first call

```
kompile --llvm-kompile-type c my_defintion.k
```

and then launch the server via

```
kore-rpc-booster ./my_defintion-kompiled/definition.kore --module MY-DEFINITION --llvm-backend-library ./my_defintion-kompiled/interpreter
```

## Building

Besides [git], you will need [stack] or [cabal] to build the project.

```sh
stack build all
# or
cabal build all
```

You may pass `--fast` to `stack build` or `-O0` to `cabal build` in order to
disable compiler optimizations and make build faster at the cost of slower runtime.

## Developing

Developers will require all the dependencies listed above,
in addition to the requirements and recommendations below.

### Required dependencies

For integration testing, we require:

- GNU [make]
- The [K Framework] frontend (its version should be the one stated in [/deps/k_release](https://github.com/runtimeverification/haskell-backend/blob/master/deps/k_release))
- Python 3.x with the [`jsonrpcclient`](https://github.com/explodinglabs/jsonrpcclient) library installed

You can install/have access to K by either:
  * using [kup]
  * using a pre-built binary (see the releases page in the K repository)
  * if you use Nix, see the section below
  * or by just building K from source

### Recommended dependencies

For setting up a development environment, we recommend:

- [direnv] to make the project's tools available in shells and editors.
- [ghcup] or [Nix] for managing required Haskell tooling
- [hlint] and [fourmolu] for compliance with project guidelines.

### Running Haskell Language Server

[ghcup] provides a straightforward way of installing the language server,
if you prefer to use [Nix] please refer to the relevant resources on how to
set up your [Nix] environment to build the server.
**Note**: HLS has to be built with the project's GHC version.

Prequisite: build the project with either Stack or Cabal.

Instructions on integrating with VSCode:
1. Install the [Haskell extension]
2. Go to `Extension Settings` and pick `GHCup` in the `Manage HLS` dropdown
3. (Optional) Manually set the GHCup executable path
4. (Extra) Set `Formatting Provider` to `fourmolu` for correct formatting

You may also need to install the [nix-env-selector](https://marketplace.visualstudio.com/items?itemName=arrterian.nix-env-selector) extension.

The `nix-env-selector` extension may prompt for the workspace to be re-loaded. Once re-loaded, HLS should start working.

### Developing with Nix

To build and run nix based packages at RV, please follow these instructions to set up nix:

_We are using [nix flakes](https://nixos.wiki/wiki/Flakes) in all our repos. What this means at a high level is that some of the commands for building packages look a bit different. See also the [runtimeverification/rv-nix-tools](https://github.com/runtimeverification/rv-nix-tools) repository._

To set up nix flakes you will need to install `nix` 2.4 or higher.If you are on a standard Linux distribution, such as Ubuntu, first [install nix](https://nixos.org/download.html#download-nix)
and then enable flakes by editing either `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` and adding:

```
experimental-features = nix-command flakes
```

Note that if this is your first time using [Nix] you will have to manually create one of the `.conf` files.

This is needed to expose the Nix 2.0 CLI and flakes support that are hidden behind feature-flags. (If you are on a different system like nixOS, see instructions for enabling flakes here: https://nixos.wiki/wiki/Flakes)

By default, Nix will build any project and its transitive dependencies from source, which can take a very long time. We therefore need to set up some binary caches to speed up the build
process. First, install cachix

```
nix profile install github:cachix/cachix/v1.1
```

and then add the `k-framework` cachix cache

```
cachix use k-framework
```

Next, we need to set up the cache for our haskell infrastructure, by adding the following sections to `/etc/nix/nix.conf` or, if you are a trusted user, `~/.config/nix/nix.conf` (if you don't know what a "trusted user" is, you probably want to do the former):

```
trusted-public-keys = ... hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
substituters = ... https://cache.iog.io
```

i.e. if the file was originally

```
substituters = https://cache.nixos.org
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

it will now read

```
substituters = https://cache.nixos.org https://cache.iog.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

Make sure that the file wasn't overwritten, if it was add the `experimental-features` again.

### Formatting
The CI requires all Haskell files to be formatted via [fourmolu](https://hackage.haskell.org/package/fourmolu).

If using VSCode, please refer to the language server section above. If not, the easiest way to do this locally is to run

```
nix run .#format
```

This will format all the haskell files in the given folder and all sub-folders. You can `cd` into a particular subfolder and run the command there, or if you only want to format a specific file, you can provide it as an argument to the above command:

```
nix run .#format Foo.hs
```

### Nix dev shell

We provide a development nix shell with a suitable development environment and
a binary cache at [runtimeverification.cachix.org]. The development can be launched via `nix develop` and then calling `stack build/test/etc`.

### Nix-direnv

Using a version of direnv that works with nix (https://github.com/nix-community/nix-direnv) allows seamless loading and unloading of the nix shell, which adds all the required packages such as `cabal`, `hpack`, `fourmolu`, etc. Use the above link to install `nix-direnv`, making sure to hook direnv into whichever shell you are using (https://direnv.net/docs/hook.html). You can use the default nix shell (currently GHC version 9.6.4) by running

```bash
echo "use flake" > .envrc
```

Finally, run `direnv allow` inside the repo folder to load up the nix shell.

Note that only `cabal` currently works within the nix shell and since it does not support the HPack `package.yaml` file format, any changes to this file will require running `hpack` before they are picked up by cabal.

### Upgrading dependencies

We use `stack.yaml` (and hence `stack.yaml.lock`) as the source of truth about the Haskell package set the project is built with. The Nix flake uses [stacklock2nix](https://github.com/cdepillabout/stacklock2nix) to make the packages specified by the lock file available to `cabal-install` inside Nix.

Any GHC or resolver upgrades must double-check the `ghcVersion` value in the [`flake.nix`](https://github.com/runtimeverification/haskell-backend/blob/master/flake.nix#L32) file.

It may also be required to update [`all-cabal-hashes`](https://github.com/runtimeverification/haskell-backend/blob/master/flake.nix#L101).

To support the scenario of building the project with `cabal-install` outside of Nix, We use a `cabal.project.freeze` file to pin the dependencies to what the current `stack` resolver is using. The script [`scripts/freeze-cabal-to-stack-resolver.sh`](https://github.com/runtimeverification/haskell-backend/tree/master/scripts/freeze-cabal-to-stack-resolver.sh) should do most of that work, and [`scripts/check-cabal-stack-sync.sh`](https://github.com/runtimeverification/haskell-backend/tree/master/scripts/check-cabal-stack-sync.sh) checks the result. Some manual adjustments will still be necessary for the `nix` builds in CI and locally to work.

### Integration tests

Haskell-backend provides an integration shell for running integration tests, which compile the K framework (of a specified version) against your current version of haskell backend and brings K into scope of your current shell. 

The integration shell is part of the `k` repository, but invoked from the local tree, adding additional tools (see [`nix/integration-shell.nix`](https://github.com/runtimeverification/haskell-backend/blob/master/nix/integration-shell.nix) and [`../k/flake.nix`](https://github.com/runtimeverification/k/blob/master/flake.nix#L180)). Its `haskell-backend` dependency must be overridden to use the `haskell-backend` dependency from the current checked-out tree, and the `k` version will usually be the one from `deps/k_release`.

To use this nix shell, execute

``` sh
me@somewhere:haskell-backend$ nix develop \
    github:runtimeverification/k/v$(cat deps/k_release)#kore-integration-tests \
    --override-input haskell-backend . --update-input haskell-backend
...
...(nix output)
integration-shell:me@somewhere:haskell-backend$
```

(This will take some time the first time you run it for a specific K framework version...)  
A specific commit or version tag of the K framework github repository can be used instead of `$(cat deps/k_release)`.

Running this command will add all of the K binaries like `kompile` or `kast` into the `PATH` of your current shell. This is not permanent and will only persist in your current shell window until you exit the active nix shell).

```
integration-shell:me@somewhere:haskell-backend$ make -C test/issue-3344 test
...
...(make output)
integration-shell:me@somewhere:haskell-backend$ exit
me@somewhere:haskell-backend$ 
```

### Integration/Performance tests of downstream projects

#### scripts/performance-tests-{kevm, kontrol, mx}.sh

Call these scripts from the root of the repo to obtain performance numbers for the KEVM and Kontrol test suites. These are necessary for any new feature which is expected to modify the perfromance of the booster and the timings should be included in the PR.


#### scripts/booster-analysis.sh

This script can be used with any folder containing bug reports to build an analysis of fallback/abort reasons in the booster. To obtain bug reports, first run `PYTEST_PARALLEL=8 scripts/performance-tests-kevm.sh --bug-report`, which will generate tarballs for all the tests and drop them into `scripts/bug-reports/`. Then call `scripts/booster-analysis.sh scripts/booster-analysis.sh scripts/bug-reports/kevm-v1.0.417-main`


### Generating an integration test from a bug-report.tar.gz

1) Rename `bug-report.tar.gz` to something more descriptive like `issue-123.tar.gz`
2) Copy the tar file into `test/rpc-integration/`
3) Run `./generateDirectoryTest.sh issue-123.tar.gz`. This will copy the definition files into `resources/` and rpc commands into `test-issue-123/`
4) Run the test via `./runDirectoryTest test-issue-123`

Note that it is also possible to run a test directly from a tarball with `runDirectoryTest.sh`, skipping the unpacking step. This is useful in automated workflows that involve running several tarballs.

### Pretty printing KORE JSON

There is a simple utility called pretty which can be used to pretty print a KORE JSON term from a bug report, which does not contain the original K definition:

```
cabal run pretty -- ../definition.kore <(jq '.result.state' XXX_response.json)
```


            

[docs]: https://github.com/runtimeverification/haskell-backend/tree/master/docs
[git]: https://git-scm.com/
[stack]: https://www.haskellstack.org/
[cabal]: https://haskell.org/cabal
[K Framework]: https://github.com/runtimeverification/k
[curl]: https://curl.haxx.se/
[make]: https://www.gnu.org/software/make/
[direnv]: https://github.com/direnv/direnv
[haskell-language-server]: https://github.com/haskell/haskell-language-server
[language server]: https://langserver.org/
[hlint]: https://github.com/ndmitchell/hlint
[fourmolu]: https://github.com/fourmolu/fourmolu
[runtimeverification.cachix.org]: https://runtimeverification.cachix.org/
[Nix]: https://nixos.org
[entr]: https://github.com/eradman/entr
[fd]: https://github.com/sharkdp/fd
[kup]: https://github.com/runtimeverification/k/releases/latest 
[ghcup]: https://www.haskell.org/ghcup/
[Haskell extension]: https://marketplace.visualstudio.com/items?itemName=haskell.haskell
[KORE RPC protocol]: ./docs/2022-07-18-JSON-RPC-Server-API.md
[pyk]: https://github.com/runtimeverification/k/tree/master/pyk
