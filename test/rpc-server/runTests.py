#!/usr/bin/env python3
from jsonrpcclient import request, request_uuid, notification
import json, socket, os, subprocess, difflib

script_path = os.path.dirname(os.path.realpath(__file__))
kore_rpc = os.path.join('.build', 'kore', 'bin', 'kore-rpc')
SERVER = os.getenv('SERVER',  os.path.join(script_path, '..', '..', kore_rpc))
HOST = "127.0.0.1"  # Standard loopback interface address (localhost)
PORT = 31337  # Port to listen on (non-privileged ports are > 1023)
CREATE_MISSING_GOLDEN = os.getenv("CREATE_MISSING_GOLDEN", 'False').lower() in ('true', '1', 't')
RECREATE_BROKEN_GOLDEN = os.getenv("RECREATE_BROKEN_GOLDEN", 'False').lower() in ('true', '1', 't')


VERBOSITY = int(os.getenv("VERBOSITY", '1'))


def debug(msg):
    if VERBOSITY > 1: print(msg)


def info(msg):
    if VERBOSITY > 0: print(msg)


server_log_level = {
    0: "error",
    1: "warning",
    2: "info"
}


def recv_all(sock):
    BUFF_SIZE = 4096 # 4 KiB
    data = b''
    while True:
        part = sock.recv(BUFF_SIZE)
        data += part
        if len(part) < BUFF_SIZE:
            # either 0 or end of data
            break
    return data


def rpc_request_id1(name, params):
    return json.dumps(
            request(
                name,
                params=params,
                id=1)
        ).encode()


def cancel():
    return json.dumps(notification("cancel")).encode()


green = '\x1b[38;5;16;48;5;2m'
red = '\x1b[38;5;16;48;5;1m'
endgreen = '\x1b[0m'
endred = '\x1b[0m'


def diff_strings(a, b):
    output = []
    matcher = difflib.SequenceMatcher(None, a, b)

    for opcode, a0, a1, b0, b1 in matcher.get_opcodes():
        if opcode == 'equal':
            output.append(a[a0:a1])
        elif opcode == 'insert':
            output.append(f'{green}{b[b0:b1]}{endgreen}')
        elif opcode == 'delete':
            output.append(f'{red}{a[a0:a1]}{endred}')
        elif opcode == 'replace':
            output.append(f'{green}{b[b0:b1]}{endgreen}')
            output.append(f'{red}{a[a0:a1]}{endred}')
    return ''.join(output)

def checkGolden (resp, resp_golden_path):
  if os.path.exists(resp_golden_path):
    debug("Checking against golden file...")
    with open(resp_golden_path, 'rb') as resp_golden_raw:
      golden_json = resp_golden_raw.read()
      if golden_json != resp:
        print(f"Test '{name}' {red}failed.{endred}")
        info(diff_strings(str(golden_json), str(resp)))
        if RECREATE_BROKEN_GOLDEN:
          with open(resp_golden_path, 'wb') as resp_golden_writer:
            resp_golden_writer.write(resp)
        else:
          info("Expected")
          info(golden_json)
          info("but got")
          info(resp)
          exit(1)
      else:
        info(f"Test '{name}' {green}passed{endgreen}")
  elif CREATE_MISSING_GOLDEN or RECREATE_BROKEN_GOLDEN:
    with open(resp_golden_path, 'wb') as resp_golden_writer:
      resp_golden_writer.write(resp)
  else:
    debug(resp)
    info(f"Golden file {red}not found{endred}")
    exit(1)



def runTest(def_path, req, resp_golden_path):
    with subprocess.Popen(f"{SERVER} {def_path} --module TEST --server-port {PORT} --log-level {server_log_level[VERBOSITY]}".split()) as process:
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
          while True:
            try:
              s.connect((HOST, PORT))
              debug("Connected to server...")
              break
            except:
              pass
          s.sendall(req)
          resp = recv_all(s)
          debug(resp)
          process.kill()

          checkGolden(resp, resp_golden_path)

print("Running execute tests:")

for name in os.listdir("./execute"):
  info(f"- test '{name}'...")
  def_path = os.path.join("./execute", name, "definition.kore")
  params_json_path = os.path.join("./execute", name, "params.json")
  state_json_path = os.path.join("./execute", name, "state.json")
  resp_golden_path = os.path.join("./execute", name, "response.golden")
  with open(params_json_path, 'r') as params_json:
    with open(state_json_path, 'r') as state_json:
      params = json.loads(params_json.read())
      state = json.loads(state_json.read())
      params["state"] = state
      req = rpc_request_id1("execute", params)
      runTest(def_path, req, resp_golden_path)

print("Running implies tests:")

for name in os.listdir("./implies"):
  info(f"- test '{name}'...")
  implies_def_path = os.path.join("./implies", name, "definition.kore")
  params_json_path = os.path.join("./implies", name, "antecedent.json")
  state_json_path = os.path.join("./implies", name, "consequent.json")
  resp_golden_path = os.path.join("./implies", name, "response.golden")
  with open(params_json_path, 'r') as antecedent_json:
    with open(state_json_path, 'r') as consequent_json:
      antecedent = json.loads(antecedent_json.read())
      consequent = json.loads(consequent_json.read())
      params = {}
      params["antecedent"] = antecedent
      params["consequent"] = consequent
      req = rpc_request_id1("implies", params)
      runTest(implies_def_path, req, resp_golden_path)


print("Running simplify tests:")

for name in os.listdir("./simplify"):
  info(f"- test '{name}'...")
  simplify_def_path = os.path.join("./simplify", name, "definition.kore")
  state_json_path = os.path.join("./simplify", name, "state.json")
  resp_golden_path = os.path.join("./simplify", name, "response.golden")
  with open(state_json_path, 'r') as state_json:
      state = json.loads(state_json.read())
      params = {}
      params["state"] = state
      req = rpc_request_id1("simplify", params)
      runTest(simplify_def_path, req, resp_golden_path)

print("Running add-module tests:")

def_path = "./add-module/add/definition.kore"

params_execute_json_path = "./add-module/execute/params.json"
resp_execute_fail_golden_path = "./add-module/execute/response-fail.golden"
resp_execute_success_golden_path = "./add-module/execute/response-success.golden"
resp_execute_defaultmodule_golden_path = "./add-module/execute/response-defaultmodule.golden"
state_execute_path = "./add-module/execute/state.json"
with open(params_execute_json_path, 'r') as params_json:
  with open(state_execute_path, 'r') as state_json:
    state = json.loads(state_json.read())
    params_execute = json.loads(params_json.read())
    params_execute["state"] = state
    req_execute = rpc_request_id1("execute", params_execute)
    req_execute_default = rpc_request_id1("execute", {"state": state})

params_add_json_path = "./add-module/add/params.json"
resp_add_golden_path = "./add-module/add/response.golden"
with open(params_add_json_path, 'r') as params_json:
  params_add = json.loads(params_json.read())
  req_add = rpc_request_id1("add-module", params_add)

with subprocess.Popen(f"{SERVER} {def_path} --module TEST --server-port {PORT} --log-level {server_log_level[VERBOSITY]}".split()) as process:
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
          while True:
            try:
              s.connect((HOST, PORT))
              debug("Connected to server...")
              break
            except:
              pass
          try:
            name = "execute-fail"
            info(f"- test '{name}'...")
            s.sendall(req_execute)
            resp = recv_all(s)
            debug(resp)
            checkGolden(resp, resp_execute_fail_golden_path)

            name = "execute-default"
            info(f"- test '{name}'...")
            s.sendall(req_execute_default)
            resp = recv_all(s)
            debug(resp)
            checkGolden(resp, resp_execute_defaultmodule_golden_path)

            name = "add-module"
            info(f"- test '{name}'...")
            s.sendall(req_add)
            resp = recv_all(s)
            debug(resp)
            checkGolden(resp, resp_add_golden_path)

            name = "execute-success"
            info(f"- test '{name}'...")
            s.sendall(req_execute)
            resp = recv_all(s)
            debug(resp)
            checkGolden(resp, resp_execute_success_golden_path)

            name = "execute-default again"
            info(f"- test '{name}'...")
            s.sendall(req_execute_default)
            resp = recv_all(s)
            debug(resp)
            checkGolden(resp, resp_execute_defaultmodule_golden_path)
          finally:
            process.kill()

print("Running get-model tests:")

for name in os.listdir("./get-model"):
  info(f"- test '{name}'...")
  get_model_def_path = os.path.join("./get-model", name, "definition.kore")
  state_json_path = os.path.join("./get-model", name, "state.json")
  resp_golden_path = os.path.join("./get-model", name, "response.golden")
  with open(state_json_path, 'r') as state_json:
      state = json.loads(state_json.read())
      params = {}
      params["state"] = state
      req = rpc_request_id1("get-model", params)
      runTest(get_model_def_path, req, resp_golden_path)

print("Running simplify-implication tests:")

for name in os.listdir("./simplify-implication"):
  info(f"- test '{name}'...")
  simplify_implication_def_path = os.path.join("./simplify-implication", name, "definition.kore")
  params_json_path = os.path.join("./simplify-implication", name, "antecedent.json")
  state_json_path = os.path.join("./simplify-implication", name, "consequent.json")
  resp_golden_path = os.path.join("./simplify-implication", name, "response.golden")
  with open(params_json_path, 'r') as antecedent_json:
    with open(state_json_path, 'r') as consequent_json:
      antecedent = json.loads(antecedent_json.read())
      consequent = json.loads(consequent_json.read())
      params = {}
      params["antecedent"] = antecedent
      params["consequent"] = consequent
      req = rpc_request_id1("simplify-implication", params)
      runTest(simplify_implication_def_path, req, resp_golden_path)
