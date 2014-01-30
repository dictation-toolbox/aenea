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

# If this is enabled, reloading the configuration will not copy over the
# client-side configuration file at util/config.py (ie this file). this is useful
# if you wish to keep the master copy of your configuration files in the Natlink
# directory.
DONT_UPDATE_CONFIG = False

# When capturing keystrokes in the client, how long to wait (in milliseconds)
# after the last keystroke before sending the text to the server.
#
# A larger value will result in higher latency and bandwidth, meaning that if you
# frequently speak long, complicated phrases they will be transmitted more
# quickly this way.  A smaller value will result in lower latency and bandwidth,
# meaning that phrases will show up more quickly after you speak them but the
# overall transmission speed will be reduced. I would experiment with values in
# the range from 2 to 20 to find a balance you like. This only affects the
# dictation capture client, not grammars in command mode.
AENEA_CLIENT_FLUSH_DELAY = 2

import os
import shutil

def reload_aenea_configuration():
  for name in os.listdir("%s\\grammars_enabled" % PROJECT_ROOT):
    if name.endswith(".py"):
      with open("%s\\grammars_enabled\\%s" % (PROJECT_ROOT, name)) as infd:
        with open("C:\\NatLink\\NatLink\\MacroSystem\\_%s" % name, "w") as outfd:
          outfd.write(infd.read())
    elif os.path.isdir("%s\\grammars_enabled\\%s" % (PROJECT_ROOT, name)):
      shutil.rmtree("C:\\NatLink\\NatLink\\MacroSystem\\%s" % name, ignore_errors=True)
      shutil.copytree("%s\\grammars_enabled\\%s" % (PROJECT_ROOT, name), "C:\\NatLink\\NatLink\\MacroSystem\\%s" % name)
  for name in os.listdir("%s\\util" % PROJECT_ROOT):
    if name == "config.py" and DONT_UPDATE_CONFIG:
      continue

    if name.endswith(".py") or name.endswith(".txt"):
      with open("%s\\util\\%s" % (PROJECT_ROOT, name)) as infd:
        with open("C:\\NatLink\\NatLink\\MacroSystem\\%s" % name, "w") as outfd:
          outfd.write(infd.read())

if  __name__ =='__main__':
  reload_aenea_configuration()
