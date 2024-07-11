#!/usr/bin/env bash
set -euxo pipefail

# Disable the Python keyring, otherwise poetry sometimes asks for password. See
#  https://github.com/pypa/pip/issues/7883
export PYTHON_KEYRING_BACKEND=keyring.backends.null.Keyring

KEVM_VERSION=${KEVM_VERSION:-'master'}

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"

MASTER_COMMIT="$(git rev-parse origin/master)"
MASTER_COMMIT_SHORT="$(git rev-parse --short origin/master)"

FEATURE_BRANCH_NAME=${FEATURE_BRANCH_NAME:-"$(git rev-parse --abbrev-ref HEAD)"}
FEATURE_BRANCH_NAME="${FEATURE_BRANCH_NAME//\//-}"

PYTEST_PARALLEL=${PYTEST_PARALLEL:-3}

if [[ $FEATURE_BRANCH_NAME == "master" ]]; then
  FEATURE_BRANCH_NAME="feature"
fi

# Create a temporary directory (or use the one provided) and store its name in a variable.
KEEP_TEMPD=${KEEP_TEMPD:-''}
FRESH_TEMPD=0
TEMPD=${TEMPD:-''}
if [ -z "$TEMPD" ]; then
    FRESH_TEMPD=1
    TEMPD=$(mktemp -d)
fi

# Exit if the temp directory wasn't created successfully.
if [ ! -e "$TEMPD" ]; then
    >&2 echo "Failed to create temp directory"
    exit 1
fi

clean_up () {
    if [ -z "$KEEP_TEMPD" ]; then
        rm -rf "$TEMPD"
    fi
    killall kore-rpc-booster || echo "no zombie processes found"
}

# Make sure the temp directory gets removed (unless KEEP_TEMPD is set) and kore-rpc-booster gets killed on script exit.
trap "exit 1"  HUP INT PIPE QUIT TERM
trap clean_up  EXIT

feature_shell() {
  GC_DONT_GC=1 nix develop . --extra-experimental-features 'nix-command flakes' --override-input k-framework/haskell-backend $SCRIPT_DIR/../ --command bash -c "$1"
}

master_shell() {
  GC_DONT_GC=1 nix develop . --extra-experimental-features 'nix-command flakes' --override-input k-framework/haskell-backend github:runtimeverification/haskell-backend/$MASTER_COMMIT --command bash -c "$1"
}

cd $TEMPD
if [[ $FRESH_TEMPD -gt 0 ]]; then
    git clone --depth 1 --branch $KEVM_VERSION https://github.com/runtimeverification/evm-semantics.git
fi
cd evm-semantics

if [[ $KEVM_VERSION == "master" ]]; then
  KEVM_VERSION=$(git name-rev --tags --name-only $(git rev-parse HEAD))
else
  KEVM_VERSION="${KEVM_VERSION//\//-}"
fi

if [[ $FRESH_TEMPD -gt 0 ]]; then
    git submodule update --init --recursive --depth 1 kevm-pyk/src/kevm_pyk/kproj/plugin
fi

BUG_REPORT=''
POSITIONAL_ARGS=()

while [[ $# -gt 0 ]]; do
  case $1 in
    --bug-report)
      mkdir -p $SCRIPT_DIR/bug-reports/kevm-$KEVM_VERSION-$FEATURE_BRANCH_NAME
      BUG_REPORT="--bug-report --bug-report-dir $SCRIPT_DIR/bug-reports/kevm-$KEVM_VERSION-$FEATURE_BRANCH_NAME"
      shift # past argument
      ;;
    -*|--*)
      echo "Unknown option $1"
      exit 1
      ;;
    *)
      POSITIONAL_ARGS+=("$1") # save positional arg
      shift # past argument
      ;;
  esac
done

set -- "${POSITIONAL_ARGS[@]}" # restore positional parameters

# kompile evm-semantics or skip kompilation if using an existing TEMPD
if [[ $FRESH_TEMPD -gt 0 ]]; then
    feature_shell "make poetry && poetry run -C kevm-pyk -- kdist --verbose build evm-semantics.plugin evm-semantics.haskell --jobs 4"
fi

# kompile all verification K definitions and specs
PREKOMPILED_DIR=$TEMPD/prekompiled
mkdir -p $PREKOMPILED_DIR
feature_shell "cd kevm-pyk && poetry run pytest src/tests/integration/test_prove.py::test_kompile_targets -vv --maxfail=0 --kompiled-targets-dir $PREKOMPILED_DIR"

mkdir -p $SCRIPT_DIR/logs

feature_shell "make test-prove-rules PYTEST_PARALLEL=$PYTEST_PARALLEL PYTEST_ARGS='--maxfail=0 --timeout 7200 -vv $BUG_REPORT --kompiled-targets-dir $PREKOMPILED_DIR' | tee $SCRIPT_DIR/logs/kevm-$KEVM_VERSION-$FEATURE_BRANCH_NAME.log"
killall kore-rpc-booster || echo "No zombie processes found"


if [ -z "$BUG_REPORT" ]; then
if [ ! -e "$SCRIPT_DIR/logs/kevm-$KEVM_VERSION-master-$MASTER_COMMIT_SHORT.log" ]; then
  master_shell "make test-prove-rules PYTEST_PARALLEL=$PYTEST_PARALLEL PYTEST_ARGS='--maxfail=0 --timeout 7200 -vv --kompiled-targets-dir $PREKOMPILED_DIR' | tee $SCRIPT_DIR/logs/kevm-$KEVM_VERSION-master-$MASTER_COMMIT_SHORT.log"
  killall kore-rpc-booster || echo "No zombie processes found"
fi

cd $SCRIPT_DIR
python3 compare.py logs/kevm-$KEVM_VERSION-$FEATURE_BRANCH_NAME.log logs/kevm-$KEVM_VERSION-master-$MASTER_COMMIT_SHORT.log > logs/kevm-$KEVM_VERSION-master-$MASTER_COMMIT_SHORT-$FEATURE_BRANCH_NAME-compare
fi
