#!/usr/bin/env bash
set -euxo pipefail

# Disable the Python keyring, otherwise poetry sometimes asks for password. See
#  https://github.com/pypa/pip/issues/7883
export PYTHON_KEYRING_BACKEND=keyring.backends.null.Keyring

KONTROL_VERSION=${KONTROL_VERSION:-'master'}

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"

MASTER_COMMIT="$(git rev-parse origin/master)"
MASTER_COMMIT_SHORT="$(git rev-parse --short origin/master)"

FEATURE_BRANCH_NAME=${FEATURE_BRANCH_NAME:-"$(git rev-parse --abbrev-ref HEAD)"}
FEATURE_BRANCH_NAME="${FEATURE_BRANCH_NAME//\//-}"

PYTEST_PARALLEL=${PYTEST_PARALLEL:-2}

if [[ $FEATURE_BRANCH_NAME == "master" ]]; then
  FEATURE_BRANCH_NAME="feature"
fi

# Create a temporary directory and store its name in a variable.
TEMPD=$(mktemp -d)
KEEP_TEMPD=${KEEP_TEMPD:-''}

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

cd $TEMPD
git clone --depth 1 --branch $KONTROL_VERSION https://github.com/runtimeverification/kontrol.git
cd kontrol
git submodule update --init --recursive --depth 1


if [[ $KONTROL_VERSION == "master" ]]; then
  KONTROL_VERSION=$(git name-rev --tags --name-only $(git rev-parse HEAD))
else
  KONTROL_VERSION="${KONTROL_VERSION//\//-}"
fi

KEVM_VERSION="v$(cat deps/kevm_release)"

BUG_REPORT=''
POSITIONAL_ARGS=()

while [[ $# -gt 0 ]]; do
  case $1 in
    --bug-report)
      mkdir -p $SCRIPT_DIR/bug-reports/kontrol-$KONTROL_VERSION-$FEATURE_BRANCH_NAME
      BUG_REPORT="--bug-report --bug-report-dir $SCRIPT_DIR/bug-reports/kontrol-$KONTROL_VERSION-$FEATURE_BRANCH_NAME"
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


# poetry takes too long to clone kevm-pyk, so we just do a shallow clone locally and override pyproject.toml
git clone --depth 1 --branch $KEVM_VERSION https://github.com/runtimeverification/evm-semantics.git
cd evm-semantics
git submodule update --init --recursive --depth 1 kevm-pyk/src/kevm_pyk/kproj/plugin

cd ..

# Patch kontrol
sed -i'' -e "s|git = \"https://github.com/runtimeverification/evm-semantics.git\", tag = \"$KEVM_VERSION\", subdirectory = \"kevm-pyk\"|path = \"evm-semantics/kevm-pyk/\"|g" pyproject.toml
sed -i'' -e "s|'forge', 'build'|'forge', 'build', '--no-auto-detect'|g" src/kontrol/foundry.py
sed -i'' -e "s|'forge', 'build'|'forge', 'build', '--no-auto-detect'|g" src/tests/utils.py
sed -i'' -e "s|'forge', 'build'|'forge', 'build', '--no-auto-detect'|g" src/tests/integration/conftest.py
# update the lock file to keep poetry from complaining
# option `--no-update` was inverted to `--regenerate` in poetry-2.0.0
poetry lock --no-update || poetry lock

feature_shell() {
  GC_DONT_GC=1 nix develop . --extra-experimental-features 'nix-command flakes' --override-input kevm/k-framework/haskell-backend $SCRIPT_DIR/../ --ignore-environment --command bash -c "$1"
}

master_shell() {
  GC_DONT_GC=1 nix develop . --extra-experimental-features 'nix-command flakes' --override-input kevm/k-framework/haskell-backend github:runtimeverification/haskell-backend/$MASTER_COMMIT --ignore-environment --command bash -c "$1"
}

# kompile Kontrol's K dependencies
feature_shell "poetry install && poetry run kdist --verbose build evm-semantics.plugin evm-semantics.haskell kontrol.foundry --jobs 4"

# kompile the test contracts, to be reused in feature_shell and master_shell. Copy the result from pytest's temp directory
PYTEST_TEMP_DIR=$TEMPD/pytest-temp-dir
mkdir -p $PYTEST_TEMP_DIR
FOUNDRY_DIR=$TEMPD/foundry
mkdir -p $FOUNDRY_DIR
feature_shell "make test-integration TEST_ARGS='--basetemp=$PYTEST_TEMP_DIR -n0 --dist=no -k test_foundry_kompile --update-expected-output'"
cp -r $PYTEST_TEMP_DIR/foundry/* $FOUNDRY_DIR

mkdir -p $SCRIPT_DIR/logs

# use special options if given, but restore KORE_RPC_OPTS afterwards
FEATURE_SERVER_OPTS=${FEATURE_SERVER_OPTS:-''}
if [ ! -z "${FEATURE_SERVER_OPTS}" ]; then
    echo "Using special options '${FEATURE_SERVER_OPTS}' via KORE_RPC_OPTS"
    if [ ! -z "${KORE_RPC_OPTS:-}" ]; then
        PRIOR_OPTS=${KORE_RPC_OPTS}
    fi
    export KORE_RPC_OPTS=${FEATURE_SERVER_OPTS}
fi

# set test arguments and select which tests to run
QUOTE='"'
TEST_ARGS="--foundry-root $FOUNDRY_DIR --maxfail=0 --numprocesses=$PYTEST_PARALLEL -vv $BUG_REPORT -k 'not (test_kontrol_cse or test_foundry_minimize_proof or test_kontrol_end_to_end)'"

feature_shell "make test-integration TEST_ARGS=$QUOTE$TEST_ARGS$QUOTE | tee $SCRIPT_DIR/logs/kontrol-$KONTROL_VERSION-$FEATURE_BRANCH_NAME.log"
killall kore-rpc-booster || echo "no zombie processes found"

if [ -z "$BUG_REPORT" ]; then
    if [ ! -z "${PRIOR_OPTS:-}" ]; then
        export KORE_RPC_OPTS=${PRIOR_OPTS}
    else
        unset KORE_RPC_OPTS
    fi
    if [ ! -e "$SCRIPT_DIR/logs/kontrol-$KONTROL_VERSION-master-$MASTER_COMMIT_SHORT.log" ]; then
      # remove proofs so that they are not reused by the master shell call
      rm -rf $FOUNDRY_DIR/out/proofs
      master_shell "make test-integration TEST_ARGS=$QUOTE$TEST_ARGS$QUOTE | tee $SCRIPT_DIR/logs/kontrol-$KONTROL_VERSION-master-$MASTER_COMMIT_SHORT.log"
      killall kore-rpc-booster || echo "no zombie processes found"
    fi
fi

cd $SCRIPT_DIR
python3 compare.py logs/kontrol-$KONTROL_VERSION-$FEATURE_BRANCH_NAME.log logs/kontrol-$KONTROL_VERSION-master-$MASTER_COMMIT_SHORT.log > logs/kontrol-$KONTROL_VERSION-master-$MASTER_COMMIT_SHORT-$FEATURE_BRANCH_NAME-compare
fi
