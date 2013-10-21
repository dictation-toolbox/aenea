# Guest-side config (Windows guest sending commands)
HOST = "192.168.56.1"
PORT = 8240

# Whether to use proxy or native (not all modules support native.)
PLATFORM = "proxy"
#PLATFORM = "windows"

# Whether to use the server's multiple_actions RPC method.
USE_MULTIPLE_ACTIONS = False

import os
import shutil

def reload_aenea_configuration():
  for name in os.listdir("E:\\aenea\\grammar"):
    if name.endswith(".py"):
      with open("E:\\aenea\\grammar\\%s" % name) as infd:
        with open("C:\\NatLink\\NatLink\\MacroSystem\\_%s" % name, "w") as outfd:
          outfd.write(infd.read())
    elif os.path.isdir("E:\\aenea\\grammar\\%s" % name):
      shutil.rmtree("C:\\NatLink\\NatLink\\MacroSystem\\%s" % name, ignore_errors=True)
      shutil.copytree("E:\\aenea\\grammar\\%s" % name, "C:\\NatLink\\NatLink\\MacroSystem\\%s" % name)
  for name in os.listdir("E:\\aenea\\util"):
    full_src = "E:\\aenea\\util\\" + name
    if name.endswith(".py") or name.endswith(".txt"):
      with open("E:\\aenea\\util\\%s" % name) as infd:
        with open("C:\\NatLink\\NatLink\\MacroSystem\\%s" % name, "w") as outfd:
          outfd.write(infd.read())
