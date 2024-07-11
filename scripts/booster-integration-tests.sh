#!/usr/bin/env bash
set -euxo pipefail

K_VERSION=$(cat deps/k_release)
PLUGIN_VERSION=$(cat deps/blockchain-k-plugin_release)
export PATH="$(nix build github:runtimeverification/k/v$K_VERSION#k.openssl.procps.secp256k1 --no-link  --print-build-logs --json | jq -r '.[].outputs | to_entries[].value')/bin:$PATH"

if [ -f cabal.project.local ]; then
    echo "[WARN] local cabal project configuration found"
fi

hpack booster && hpack dev-tools
cabal update
cabal test llvm-integration predicates-integration

# The runDirectoryTest.sh script expects the following env vars to be set
export PLUGIN_DIR=$(nix build github:runtimeverification/blockchain-k-plugin/$PLUGIN_VERSION --no-link --json | jq -r '.[].outputs | to_entries[].value')
export SECP256K1_DIR=$(nix build nixpkgs#secp256k1 --no-link --json | jq -r '.[].outputs | to_entries[].value')

cabal build all
KORE_RPC_BOOSTER=$(cabal exec which kore-rpc-booster)
BOOSTER_DEV=$(cabal exec which booster-dev)
KORE_RPC_DEV=$(cabal exec which kore-rpc-dev)
export CLIENT=$(cabal exec which kore-rpc-client)

cd booster/test/rpc-integration
for dir in $(ls -d test-*); do
    name=${dir##test-}
    echo "Running $name..."
    case "$name" in
        "a-to-f" | "diamond")
            SERVER=$BOOSTER_DEV ./runDirectoryTest.sh test-$name $@
            SERVER=$KORE_RPC_DEV ./runDirectoryTest.sh test-$name $@
            SERVER=$KORE_RPC_BOOSTER ./runDirectoryTest.sh test-$name $@
            ;;
        "substitutions" | "vacuous" | "pathological-add-module")
            SERVER=$KORE_RPC_DEV ./runDirectoryTest.sh test-$name $@
            SERVER=$KORE_RPC_BOOSTER ./runDirectoryTest.sh test-$name $@
            ;;
        "get-model" | "collectiontest")
            SERVER=$BOOSTER_DEV ./runDirectoryTest.sh test-$name $@
            SERVER=$KORE_RPC_BOOSTER ./runDirectoryTest.sh test-$name $@
            ;;
        "condition-filtering")
            SERVER=$KORE_RPC_BOOSTER ./runDirectoryTest.sh test-$name $@
            SERVER=$BOOSTER_DEV SERVER_OPTS="--no-smt" ./runDirectoryTest.sh test-$name $@
            ;;
        "questionmark-vars")
            SERVER=$BOOSTER_DEV ./runDirectoryTest.sh test-$name $@
            SERVER=$KORE_RPC_DEV ./runDirectoryTest.sh test-$name $@
            ;;
        "use-path-condition-in-equations" | "compute-ceil" | "no-evaluator" | "non-linear-int-requires" | "get-model-subsorts" | "simplify")
            SERVER=$BOOSTER_DEV ./runDirectoryTest.sh test-$name $@
            ;;
        "log-simplify-json")
            SERVER=$KORE_RPC_BOOSTER SERVER_OPTS="--log-format json -l Simplify --log-file test-$name/simplify-log.txt" ./runDirectoryTest.sh test-$name $@
            ;;
        "foundry-bug-report")
            SERVER=$KORE_RPC_BOOSTER ./runDirectoryTest.sh test-$name --time $@
            SERVER=$KORE_RPC_BOOSTER SERVER_OPTS="--interim-simplification 100" ./runDirectoryTest.sh test-$name $@
            ;;
        "imp")
            SERVER=$KORE_RPC_BOOSTER ./runDirectoryTest.sh test-$name --time $@
            ;;
        *)
            SERVER=$KORE_RPC_BOOSTER ./runDirectoryTest.sh test-$name $@
            ;;
    esac
done
