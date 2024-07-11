#!/usr/bin/env bash
set -euxo pipefail

# Environment variables:
#   LOG_DIR: path to bug report run logs, defaults to $BUG_REPORT_DIR-logs
#   PARALLEL: number of bug reports to process in parallel, defaults to $(nproc)
#   SERVER_OPTS: additional options for the server (default: none)
#   K_VERSION: K version to use for testing (default: from deps/k_release)

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"


PARALLEL=${PARALLEL:-$(nproc)}

SERVER_OPTS=${SERVER_OPTS:-}

BUG_REPORT_DIR=$1

nix_shell() {
  GC_DONT_GC=1 nix develop $SCRIPT_DIR/..#cabal --extra-experimental-features 'nix-command flakes' --command bash -c "$1"
}

nix_shell "cabal build kore-rpc-booster"

export SERVER=$(nix_shell "cabal exec which kore-rpc-booster" | tail -1)

nix_shell "cabal build kore-rpc-client"

export CLIENT=$(nix_shell "cabal exec which kore-rpc-client" | tail -1)

nix_shell "cabal build count-aborts"

COUNT=$(nix_shell "cabal exec which count-aborts" | tail -1)

# removes trailing "/" from BUG_REPORT_DIR
export LOG_DIR=${LOG_DIR:-"$(dirname "$BUG_REPORT_DIR/.")-logs"}

mkdir -p $LOG_DIR

K_VERSION=${K_VERSION:-$(cat $SCRIPT_DIR/../deps/k_release)}
export PATH="$(nix build github:runtimeverification/k/v$K_VERSION#k.openssl.procps.secp256k1 --no-link --print-build-logs --json | jq -r '.[].outputs | to_entries[].value')/bin:$PATH"
PLUGIN_VERSION=$(cat $SCRIPT_DIR/../deps/blockchain-k-plugin_release)
export PLUGIN_DIR=$(nix build github:runtimeverification/blockchain-k-plugin/$PLUGIN_VERSION --no-link --json | jq -r '.[].outputs | to_entries[].value')



run_tarball(){
  echo "######## $1 ########";
  $SCRIPT_DIR/run-with-tarball.sh "$1" -l Aborts --log-format json --log-file "$LOG_DIR/$(basename "$1").json.log" -l Timing ${SERVER_OPTS} 2>&1 | tee "$LOG_DIR/$(basename "$1").out";
}

export -f run_tarball
export SCRIPT_DIR

find $BUG_REPORT_DIR -name \*.tar -print0 -or -name \*.tar.gz -print0 | xargs -0 -t -I {} -P $PARALLEL bash -c 'run_tarball "$@"' $(basename {}) {}

cd $LOG_DIR

# Counting abort reasons
find . -name \*.json.log | xargs $COUNT > abort_reasons.count
