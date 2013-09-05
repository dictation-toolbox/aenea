"""provides proxy contexts for currently active application matching"""

import proxy
import types

try:
  import dragonfly
except ImportError:
  import dragonfly_mock as dragonfly

communications = proxy.communications

class ProxyAppContext(dragonfly.Context):
  """matches based on the properties of the currently active window.
     use subclasses ProxyAppContextOr, ProxyAppContextAnd."""
  def __init__(self,
               name = None,
               window_class = None,
               window_class_name = None,
               max_depth = None,
               visible = None,
               pid = None,
               screen = None,
               desktop = None):
    arguments = []
    if window_class is not None:
      arguments.append("class \"%s\"" % window_class.replace('"', '\"'))
    if window_class_name is not None:
      arguments.append("classname \"%s\"" % window_class_name.replace('"', '\"'))
    if max_depth is not None:
      arguments.append("maxdepth %i" % int(max_depth))
    if visible is not None:
      arguments.append("onlyvisible")
    if name is not None:
      arguments.append("name \"%s\"" % name.replace('"', '\"'))
    if pid is not None:
      arguments.append("pid %i" % int(pid))
    if screen is not None:
      arguments.append("screen %i" % int(screen))
    if desktop is not None:
      arguments.append("desktop %i" % int(desktop))
    assert len(arguments) == 1
    dragonfly.Context.__init__(self)
    self.arguments = ["--%s" % argument for argument in arguments]
    self._custom_parse()
    self._str = "".join(self.arguments)

  def _custom_parse(self):
    pass

  def _get_proxy_matches(self):
    with communications as proxy:
      response = proxy.callReadRawCommand("search " + " ".join(self.arguments))
      window_id, window_title = proxy.callGetActiveWindow()
      return ([int(matching) for matching in response.split("\n") if matching.strip()],
              window_id,
              window_title)

  def matches(self, windows_executable, windows_title, windows_handle):
    matching_windows, window_id, window_title = self._get_proxy_matches()
    return window_id is not None and window_id in matching_windows

def ProxyAppContextOr(self, **kw):
  items = kw.items()
  value = ProxyAppContext(*dict([items[0]]))
  for (key, value) in items:
    value |= ProxyAppContext(*dict([value]))
  return value

def ProxyAppContextAnd(self, **kw):
  items = kw.items()
  value = ProxyAppContext(*dict([items[0]]))
  for (key, value) in items:
    value &= ProxyAppContext(*dict([value]))
  return value

class ProxyAnyWindowActive(dragonfly.Context):
  def __init__(self):
    self._str = "proxy any window open"

  def matches(self, windows_executable, windows_title, windows_handle):
    window_id, window_title = proxy.callGetActiveWindow()
    return window_id is not None

class ProxyNoWindowActive(dragonfly.Context):
  def __init__(self):
    self._str = "proxy no windows open"

  def matches(self, windows_executable, windows_title, windows_handle):
    with communications as proxy:
      window_id, window_title = proxy.callGetActiveWindow()
      return window_id is None

__all__ = ["ProxyAppContext", "ProxyAppContextAnd", "ProxyAppContextOr", "ProxyAnyWindowActive", "ProxyNoWindowActive"]
