#!/usr/bin/env bash

# Run RPC integration tests using files in a given directory.
# The directory name is expected to match "test-<stem>"
#
# - starts an RPC server (SERVER)
#   - with a kore definition resources/<stem>.kore
#   - if it exists, using an llvm library resources/<stem>.dylib
# - sends the files named test-<stem>/state-<test>.<suffix> as requests
#   - using CLIENT
#   - the file suffix indicates how to send the file:
#     - for suffix ".send", send the json as-is
#     - for suffix "execute", send an "execute" request, with additional
#       parameters from file test-<stem>/params-<test>.json
#     - for suffix "simplify", send a "simplify" request, with additional
#       parameters from file test-<stem>/params-<test>.json
#   - expecting the response to match test-<stem>/response-<test>.json
#
# Environment variables
#   SERVER:      path to RPC server executable
#                  (default <top>/.build/booster/bin/booster)
#   CLIENT:      path to RPC client executable
#                  (default <top>/.build/booster/bin/kore-rpc-client)
#   SERVER_OPTS: additional options to pass to the SERVER
#                  (default: none)
#   CONNECTION_ATTEMPTS: attempts to establish a connection with the server
#                        after sleepeng for 2 seconds
#                          (default: 20)

directory=${1?"Please provide a test directory in a single argument"}

set -eu

pushd $(dirname $0)
shift

# mangle kore client arguments

dir=$(basename $directory)
bindir=../../../.build/kore/bin

server=${SERVER:-$bindir/kore-rpc-booster}
client=${CLIENT:-$bindir/kore-rpc-client}
connection_attempts=${CONNECTION_ATTEMPTS:-20}

kore=resources/${dir#test-}.kore
kompile=resources/${dir#test-}.kompile
dylib=resources/${dir#test-}.dylib

echo "Running tests from $dir with definition $kore"

# make sure directory and kore file exist
[ ! -e "./$dir" ] && exit 1
if [ ! -f "./$kore" ]; then
  [ ! -f "./$kompile" ] && exit 2
  cd resources
  ./${dir#test-}.kompile
  cd ..
  [ ! -f "./$kore" ] && exit 3
fi

server_params="--server-port 0 "

if [ -f "./$dylib" ]; then
    server_params+="--llvm-backend-library ./$dylib ${SERVER_OPTS:-""}"
else
    server_params+=${SERVER_OPTS:-""}
fi

MODULE=$(grep -o -e "^module [A-Z0-9-]*" ./$kore | tail -1 | sed -e "s/module //")

echo "Starting server: $server $kore --module ${MODULE:-UNKNOWN} $server_params"
$server $kore --module ${MODULE?"Unable to find main module"} $server_params &
server_pid=$!

trap 'kill -2 ${server_pid}; popd' ERR EXIT
echo "Server PID ${server_pid}"

i=$connection_attempts
while ! lsof -a -p${server_pid} -sTCP:LISTEN -iTCP && [[ $i -ge 0 ]]; do
    echo "Waiting for server ($i attempts left)"
    i=$((i-1))
    sleep 2
done

# find server port via lsof
server_port=$(lsof -a -p${server_pid} -sTCP:LISTEN -iTCP | grep ${server_pid} | sed -e 's/.* TCP \*:\([0-9]*\).*$/\1/')

if [ -z "${server_port}" ]; then
    echo "Unable to identify the server, aborting"
    exit 4
else
    echo "Server listening on port ${server_port}"
fi

client="$client -p ${server_port}"

if [ -d $dir ] && [ -f "${dir}/test.sh" ]; then
    echo "shell-scripted test, running $dir/testsh as-is"
    . ./${dir}/test.sh
elif [ -d $dir ]; then
    echo "Directory test"
    command=""
    pretty_command=""
    for test in $( ls $dir/state-*.{execute,send,simplify,add-module,get-model} 2>/dev/null ); do
        tmp=${test#$dir/state-}
        testname=${tmp%.*}
        # determine send mode from suffix
        mode=${test##*.}
        printf "########## Test: %10s %20s\n" "$mode" "$testname #######"
        if [ -f "$dir/params-${testname}.json" ]; then
            params="--param-file $dir/params-${testname}.json"
        else
            params=""
        fi
        # build rpc-client call
        if [ $(basename $server) == "booster-dev" ] && [ -f "$dir/response-${testname}.booster-dev" ]; then
            command+=" $mode $test $params --expect $dir/response-${testname}.booster-dev $*"
            pretty_command+=" \ \n  $mode $test $params --expect $dir/response-${testname}.booster-dev $*"
        elif [ $(basename $server) == "kore-rpc-dev" ] && [ -f "$dir/response-${testname}.kore-rpc-dev" ]; then
            command+=" $mode $test $params --expect $dir/response-${testname}.kore-rpc-dev $*"
            pretty_command+=" \ \n  $mode $test $params --expect $dir/response-${testname}.kore-rpc-dev $*"
        else
            command+=" $mode $test $params --expect $dir/response-${testname}.json $*"
            pretty_command+=" \ \n  $mode $test $params --expect $dir/response-${testname}.json $*"
        fi
    done
    echo -e "$client $pretty_command"
    $client $command
else
    echo "$dir is a file, running a tarball test"
    $client run-tarball $dir
fi
