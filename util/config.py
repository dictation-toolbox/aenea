# Guest-side config (Windows guest sending commands)
HOST = "192.168.56.1"
PORT = 8240

# Whether to use proxy or native (not all modules support native.)
PLATFORM = "proxy"
#PLATFORM = "windows"

# Whether to use the server's multiple_actions RPC method.
USE_MULTIPLE_ACTIONS = True

SCREEN_RESOLUTION = (1920 * 2 + 2560), 1440

PROJECT_ROOT = "E:\\aenea"

import os
import shutil

def reload_aenea_configuration():
  for name in os.listdir("%s\\grammar" % PROJECT_ROOT):
    if name.endswith(".py"):
      with open("%s\\grammar\\%s" % (PROJECT_ROOT, name)) as infd:
        with open("C:\\NatLink\\NatLink\\MacroSystem\\_%s" % name, "w") as outfd:
          outfd.write(infd.read())
    elif os.path.isdir("%s\\grammar\\%s" % (PROJECT_ROOT, name)):
      shutil.rmtree("C:\\NatLink\\NatLink\\MacroSystem\\%s" % name, ignore_errors=True)
      shutil.copytree("%s\\grammar\\%s" % (PROJECT_ROOT, name), "C:\\NatLink\\NatLink\\MacroSystem\\%s" % name)
  for name in os.listdir("%s\\util" % PROJECT_ROOT):
    full_src = "%s\\util\\" % PROJECT_ROOT + name
    if name.endswith(".py") or name.endswith(".txt"):
      with open("%s\\util\\%s" % (PROJECT_ROOT, name)) as infd:
        with open("C:\\NatLink\\NatLink\\MacroSystem\\%s" % name, "w") as outfd:
          outfd.write(infd.read())

if  __name__ =='__main__':
  reload_aenea_configuration()