#!/usr/bin/python

import comsat, sys, os, time

class Handler(object):
  @staticmethod
  def runCommand(command):
    command_string = "xdotool %s" % command
    sys.stderr.write(command_string + "\n")
    os.system(command_string)
  
  @staticmethod
  def readCommand(command):
    with os.popen("xdotool %s" % command, "r") as fd:
      rval = fd.read()
    sys.stderr.write("xdotool %s > %s\n" % (command, rval))
    return rval
  
  @staticmethod
  def writeCommand(message):
    with os.popen("xdotool type --file -", "w") as fd:
      fd.write(message)
    sys.stderr.write("echo \"%s\" | xdotool type --file -\n" % message.replace("\n", "\\n"))

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
    window_id = int(self.readCommand("getactivewindow"))
    window_title = self.readCommand("getwindowname %ii" % window_id)
    return window_id, window_title

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
      else:
        command.append("key " + key)

    self.runCommand(' '.join(command))

  def callRaw(self, arguments):
    self.runCommand(' '.join(arguments))

  def callReloadConfiguration(self):
    pass
#    self.callRaw(["keydown Alt_L", "key space", "keyup Alt_L", 
#                           "sleep 0.05",
#                           "keydown Alt_L", "key k", "keydown Alt_L", "key 1"])

  def callGetState(self):
    state = self.state.copy()
    active_id = self.callGetActiveWindow()
    state["active"] = active_id
    return state

cs = comsat.ComSat()
cs.handlers.append(Handler())

loop = cs.serverMainLoop()
while 1:
  loop.next()
