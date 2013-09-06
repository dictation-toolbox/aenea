"""provides proxy contexts for currently active application matching"""

import proxy
import types

try:
  import dragonfly
except ImportError:
  import dragonfly_mock as dragonfly

communications = proxy.communications

DONT_CARE = object()

class ProxyBaseAppContext(dragonfly.Context):
  """matches based on the properties of the currently active window.
     see also ProxyAppContextOr, ProxyAppContextAnd."""
  def __init__(self,
               window_class = DONT_CARE,
               window_class_name = DONT_CARE,
               window_title = DONT_CARE,
               role = DONT_CARE,
               pid = DONT_CARE,
               type = DONT_CARE,
               locale = DONT_CARE,
               client_machine = DONT_CARE):
    # a little hackery is worth it for an easier to use API.
    self.arguments = {}
    for key in ("window_class", "window_class_name", "window_title", "role",
                "pid", "type", "locale", "client_machine"):
      value = locals()[key]
      if value is not DONT_CARE:
        self.arguments[key] = value
    self._str = "ProxyBaseAppContext"

  def _check_properties(self):
    with communications as proxy:
      properties = proxy.callGetCurrentWindowProperties()
    matches = {}
    for (key, value) in self.arguments.iteritems():
      matches[key] = False
      if (key in properties and
          self._property_match(properties[key], self.arguments[key])):
        matches[key] = True
    return matches

  def _property_match(self, actual, desired):
    """overload to change how we should compare actual and desired properties"""
    return actual == desired

  def _reduce_matches(self, matches):
    """overload to change the logic that should be used to combine the results
       of the matching function"""
    return all(matches.itervalues())

  def matches(self, windows_executable, windows_title, windows_handle):
    return self._reduce_matches(self._check_properties())

# and is the default behavior of the default class, but we provide this
# class anyway it to improve interface consistency.
class ProxyAppContextAnd(ProxyBaseAppContext):
  pass

class ProxyAppContext(ProxyBaseAppContext):
  pass

class ProxyAppContextOr(ProxyBaseAppContext):
  def _reduce_matches(self, matches):
    return any(matches.itervalues())

__all__ = ["ProxyAppContextAnd", "ProxyAppContextOr"]
