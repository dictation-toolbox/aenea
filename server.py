#!/usr/bin/python

import comsat, sys, os, random

# to help see when the server has started while in a bash loop
for i in range(random.randint(1, 10)):
  print

XPROP_PROPERTIES = {
    "_NET_WM_DESKTOP(CARDINAL)":"desktop",
    "WM_WINDOW_ROLE(STRING)":"role",
    "_NET_WM_WINDOW_TYPE(ATOM)":"type",
    "_NET_WM_PID(CARDINAL)":"pid",
    "WM_LOCALE_NAME(STRING)":"locale",
    "WM_CLIENT_MACHINE(STRING)":"client_machine",
    "WM_NAME(STRING)":"name"
    }

XDOTOOL_COMMAND_BREAK = set(("type",))

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
    return rval
  
  @staticmethod
  def writeCommand(message, executable="xdotool"):
    with os.popen("%s type --file -" % executable, "w") as fd:
      fd.write(message)
    sys.stderr.write("echo \"%s\" | %s type --file -\n" % (message.replace("\n", "\\n"), executable))

  def callGetCurrentWindowProperties(self):
    window_id, window_title = self.callGetActiveWindow()
    if window_id is None:
      return {}

    properties = {}
    for line in self.readCommand("-id %s" % window_id, "xprop").split("\n"):
      split = line.split(" = ", 1)
      if len(split) == 2:
        rawkey, value = split
        if split[0] in XPROP_PROPERTIES:
          properties[XPROP_PROPERTIES[rawkey]] = value[1:-1] if "(STRING)" in rawkey else value
        elif rawkey == "WM_CLASS(STRING)":
          window_class_name, window_class = value.split('", "')
          properties["window_class_name"] = window_class_name[1:]
          properties["window_class"] = window_class[:-1]

    return properties

  def callText(self, message):
    """Types a string as is."""
    if message:
      self.writeCommand(message)

  def callGetActiveWindow(self):
    """Returns the window id and title of the active window."""
    window_id = self.readCommand("getactivewindow")
    if window_id:
      window_id = int(window_id)
      window_title = self.readCommand("getwindowname %i" % window_id).strip()
      return window_id, window_title
    else:
      return None, None

  def callReloadConfiguration(self):
    pass

  def callGetState(self):
    state = self.state.copy()
    active_id, active_title = self.callGetActiveWindow()

    state["active_id"] = active_id
    state["active_title"] = active_title

    if active_id:
      try:
        active_pid = int(self.readCommand("getwindowpid %i" % active_id))
      except Exception:
        active_pid = -1
      psocks = self.readCommand("aux | grep %i" % active_pid, executable="ps")
      state["in_terminal"] = ("urxvt" in psocks or "xfce4-terminal" in psocks)
    else:
      state["in_terminal"] = False

    return state

  def callGetGeometry(self, window_id=None):
    if window_id is None:
      window_id, _ = self.callGetActiveWindow()
    geo = dict([val.lower() for val in line.split("=")]
               for line in self.readCommand(("getwindowgeometry --shell %i"
                                             % window_id)).strip().split("\n"))
    geo = dict((key, int(value)) for (key, value) in geo.iteritems())
    return geo["x"], geo["y"], geo["width"], geo["height"], geo["screen"]

  def _transform_relative_mouse_event(self, event):
    x, y, width, height, screen = self.callGetGeometry()
    dx, dy = map(int, map(float, event.split()))
    return [("mousemove", "%i %i" % (x + dx, y + dy))]

  def callExecute(self, events):
    """Execute a sequence of xdotool-style events using xdotool."""
    transformed_events = [[]]
    for (command, arguments) in events:
      if command == "mousemove_active":
        transformed_events[-1].extend(self._transform_relative_mouse_event(arguments))
      elif command in XDOTOOL_COMMAND_BREAK:
        transformed_events.append([(command, arguments)])
        transformed_events.append([])
      else:
        transformed_events[-1].append((command, arguments))

    for events in filter(None, transformed_events):
      if events[0][0] == "type":
        self.writeCommand(events[0][1])
      else:
        self.readCommand(' '.join("%s %s" % event for event in events))

  def callReadRawCommand(self, event, command="xdotool"):
    return self.readCommand(event, command)

cs = comsat.ComSat()
cs.handlers.append(Handler())

loop = cs.serverMainLoop()
while 1:
  loop.next()
