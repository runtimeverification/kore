#! /usr/bin/env bash
set -exo pipefail

# Starts an rpc server and runs a tarball against it using rpc-client
# Environment variables:
#   DEFINITION: kore file to load (optional, otherwise from tarball)
#   LLVM_LIB: default llvm library to load (provide this or PLUGIN_DIR)
#   PLUGIN_DIR: dir for blockchain plugin (optional, to compile an LLVM library)
#   BOOSTER: booster home directory (only relevant if not setting the following)
#   SERVER: kore-rpc server path (default: from build dir in BOOSTER)
#   CLIENT: rpc client path (default: from build dir in BOOSTER)
#   LOG_DIR: path to place logs, defaults to current directory

tarball=${1?"Tarball argument missing"}
shift

tarname=$(basename "$tarball")

booster=${BOOSTER:-$(realpath $(dirname $0)/..)}
server=${SERVER:-$booster/.build/kore/bin/kore-rpc-booster}
client=${CLIENT:-$booster/.build/kore/bin/kore-rpc-client}
log_dir=${LOG_DIR:-.}

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
# needed for the MX backend otherwise llvm-backend-matching crashes
export K_OPTS="$K_OPTS -Xmx16384m"

nix_shell() {
  GC_DONT_GC=1 nix develop $SCRIPT_DIR/..#cabal --extra-experimental-features 'nix-command flakes' --command bash -c "$1"
}

if [ -z "$(which lsof)" ]; then
LSOF=$(nix_shell "which lsof")
else
LSOF=$(which lsof)
fi

TEMPD=$(mktemp -d)
# Exit if the temp directory wasn't created successfully.
if [ ! -e "$TEMPD" ]; then
    >&2 echo "Failed to create temp directory"
    exit 1
fi

# Make sure the temp directory gets removed on script exit.
trap "exit 1"           HUP INT PIPE QUIT TERM
trap 'rm -rf "$TEMPD"'  EXIT


if [ -z "$DEFINITION" ]; then
    # unpack definition file from tarball, into $TEMPD dir
    tar xf "$tarball" -O definition.kore > $TEMPD/definition.kore
    kore=$TEMPD/definition.kore
else
    kore=$DEFINITION
fi

if [ ! -s $kore ]; then
    echo "Definition file $kore missing/empty"
    exit 1
fi


# find out module name from definition file
MODULE=$(grep -o -e "^module [A-Z0-9-]*" $kore | tail -1 | sed -e "s/module //")

# build llvm backend unless provided
if [ -z "${LLVM_LIB}" ]; then
    tar xf "$tarball" -O llvm_definition/definition.kore > $TEMPD/llvm-definition.kore

    LLVM_DEFINITION_SHA=$(sha1sum $TEMPD/llvm-definition.kore | awk '{ print $1 }')

    case "$OSTYPE" in
        linux*)
            LPROCPS="-lprocps"
            LIBSUFFIX="so"
            ;;
        *)
            LPROCPS=""
            LIBSUFFIX="dylib"
            ;;
    esac

    if [ ! -f "$log_dir/$LLVM_DEFINITION_SHA.$LIBSUFFIX" ]; then
        if [ ! -d "${PLUGIN_DIR}" ]; then
            echo "Either LLVM_LIB or PLUGIN_DIR must be provided"
            exit 2
        fi
        #generate matching data
        (cd $TEMPD && mkdir -p dt && llvm-kompile-matching llvm-definition.kore qbaL ./dt 1/2)
        # find library dependencies and source files
        for lib in libff libcryptopp blake2; do
            LIBFILE=$(find ${PLUGIN_DIR} -name "${lib}.a" | head -1)
            [ -z "$LIBFILE" ] && (echo "[Error] Unable to locate ${lib}.a"; exit 1)
            PLUGIN_LIBS+="$LIBFILE "
            PLUGIN_INCLUDE+="-I$(dirname $LIBFILE)/../include "
        done
        PLUGIN_CPP="${PLUGIN_DIR}/include/plugin-c/crypto.cpp ${PLUGIN_DIR}/include/plugin-c/plugin_util.cpp"

        # kompile llvm-definition to interpreter
        llvm-kompile $TEMPD/llvm-definition.kore $TEMPD/dt c -- \
                -fPIC -std=c++17 -o $TEMPD/interpreter \
                $PLUGIN_LIBS $PLUGIN_INCLUDE $PLUGIN_CPP \
                -lcrypto -lssl $LPROCPS
        lib=$TEMPD/interpreter.$LIBSUFFIX
        cp $TEMPD/interpreter.$LIBSUFFIX $log_dir/$LLVM_DEFINITION_SHA.$LIBSUFFIX
    else
        lib=$log_dir/$LLVM_DEFINITION_SHA.$LIBSUFFIX
    fi
else
    lib=$(realpath ${LLVM_LIB})
fi
if [ ! -f "$lib" ]; then
    echo "Problem with LLVM backend library at $lib"
    exit 2
fi
server_params="--llvm-backend-library $lib --server-port 0 $@"

echo "Starting server: $server $kore --module ${MODULE:-UNKNOWN} $server_params"
$server $kore --module ${MODULE?"Unable to find main module"} $server_params > ${log_dir}/${tarname}.log 2>&1 &
server_pid=$!

trap 'kill -2 ${server_pid}' ERR EXIT
echo "Server PID ${server_pid}"

i=15
while ! $LSOF -a -p${server_pid} -sTCP:LISTEN -iTCP && [[ $i -ge 0 ]]; do
    echo "Waiting for server ($i attempts left)"
    i=$((i-1))
    sleep 2
done

# find server port via lsof
server_port=$($LSOF -a -p${server_pid} -sTCP:LISTEN -iTCP | grep ${server_pid} | sed -e 's/.* TCP \*:\([0-9]*\).*$/\1/')
echo "Server listening on port ${server_port}"


echo "Running requests from $tarball against the server: $client run-tarball '$tarball' --keep-going --omit-details -p ${server_port} -h 127.0.0.1"
$client run-tarball "$tarball" --keep-going --omit-details -p ${server_port} -h 127.0.0.1

echo "Done with '$tarball'"
