#!/usr/bin/python

import comsat, sys, os, time

class Handler(object):
  def __init__(self):
    self.state = {}

  @staticmethod
  def runCommand(command, executable="xdotool"):
    command_string = "%s %s" % (executable, command)
    sys.stderr.write(command_string + "\n")
    os.system(command_string)
  
  @staticmethod
  def readCommand(command, executable="xdotool"):
    with os.popen("%s %s" % (executable, command), "r") as fd:
      rval = fd.read()
#    sys.stderr.write("%s %s > %s\n" % (executable, command, rval))
    return rval
  
  @staticmethod
  def writeCommand(message, executable="xdotool"):
    with os.popen("%s type --file -" % executable, "w") as fd:
      fd.write(message)
    sys.stderr.write("echo \"%s\" | %s type --file -\n" % (message.replace("\n", "\\n"), executable))

  def callLog(self, message):
    sys.stderr.write(message + "\n")

  def callText(self, message):
    """Types a string as is."""
    self.writeCommand(message)

  def callMouse(self, x, y, absolute=True):
    """Moves the mouse to the specified coordinates."""
    if absolute:
      self.runCommand("mousemove %i %i" % (x, y))
    else:
      self.runCommand("mousemove_relative %i %i" % (x, y))

  def callKeys(self, keys):
    """Presses keys, then releases them in opposite order."""
    if isinstance(keys, basestring):
      keys = keys.split()
    self.runCommand(' '.join("key %s" % key for key in keys))

  def callGetActiveWindow(self):
    """Returns the window id and title of the active window."""
    window_id = self.readCommand("getactivewindow")
    if window_id:
      window_id = int(window_id)
      window_title = self.readCommand("getwindowname %i" % window_id)
      return window_id, window_title
    else:
      return None, None

  def callSetIonWorkspace(self, workspace):
    """Set the current ion workspace to a number from 1 to 6"""
    mapping = [1, 2, 3, "apostrophe", "comma", "period"]
    self.callModifiedKeys(["&" + str(mapping[workspace - 1])])

  def callSetIonTab(self, tab):
    self.callModifiedKeys(["&k", str(tab)])

  def callPhantomClick(self, x, y, button=1, phantom=True):
    phantom = "mousemove restore" if phantom else ""
    self.runCommand("mousemove %i %i click %i %s" % (x, y, button, phantom))

  def callClick(self, button=1):
    self.runCommand("click %i" % button)

  def callModifiedKeys(self, keys):
    if isinstance(keys, basestring):
      keys = keys.split()

    command = []

    for key in keys:
      if key.startswith("^"):
        command.extend(("keydown Shift", "key " + key[1:], "keyup Shift"))
      elif key.startswith("&"):
        command.extend(("keydown Alt_L", "key " + key[1:], "keyup Alt_L"))
      elif key.startswith("*"):
        command.extend(("keydown Control_L", "key " + key[1:], "keyup Control_L"))
      else:
        command.append("key " + key)

    self.runCommand(' '.join(command))

  def callRaw(self, arguments):
    return self.readCommand(' '.join(arguments))

  def callReloadConfiguration(self):
    pass
#    self.callRaw(["keydown Alt_L", "key space", "keyup Alt_L", 
#                           "sleep 0.05",
#                           "keydown Alt_L", "key k", "keydown Alt_L", "key 1"])

  def callGetState(self):
    state = self.state.copy()
    active_id, active_title = self.callGetActiveWindow()

    state["active_id"] = active_id
    state["active_title"] = active_title

    if active_id:
      try:
        active_pid = int(self.callRaw(["getwindowpid %i" % active_id]))
      except:
        active_pid = -1
      state["in_terminal"] = ("urxvt" in self.readCommand("aux | grep %i" % active_pid, executable="ps"))
    else:
      state["in_terminal"] = False

    return state

cs = comsat.ComSat()
cs.handlers.append(Handler())

loop = cs.serverMainLoop()
while 1:
  loop.next()
