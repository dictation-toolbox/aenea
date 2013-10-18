"""provides proxy contexts for currently active application matching"""

import re
import time
import types

import communications
import config

communication = communications.Proxy(config.HOST, config.PORT)

try:
  import dragonfly
except ImportError:
  import dragonfly_mock as dragonfly

DONT_CARE = object()

# Hate to do this, but currently we hit the server once per context,
# and the get_context() call is actually rather expensive. Ideally we
# could propagate state through Dragonfly contexts, but that would involve
# deep surgery or re-implementation.
last_context = None
last_context_time = 0
_STALE_CONTEXT_DELTA = 0.01

class AlwaysContext(dragonfly.Context):
  def matches(self, windows_executable, windows_title, windows_handle):
    return True

class NeverContext(dragonfly.Context):
  def matches(self, windows_executable, windows_title, windows_handle):
    return False

class ProxyBaseAppContext(dragonfly.Context):
  """matches based on the properties of the currently active window.
     see also ProxyAppContextOr, ProxyAppContextAnd."""
  def __init__(self,
               cls = DONT_CARE,
               cls_name = DONT_CARE,
               name = DONT_CARE,
               role = DONT_CARE,
               pid = DONT_CARE,
               title = DONT_CARE,
               executable = DONT_CARE,
               type = DONT_CARE,
               locale = DONT_CARE,
               client_machine = DONT_CARE):
    # a little hackery is worth it for an easier to use API.
    self.arguments = {}
    for key in ("cls", "cls_name", "name", "role", "title", "executable",
                "pid", "type", "locale", "client_machine"):
      value = locals()[key]
      if value is not DONT_CARE:
        self.arguments[key] = value
    self._str = "ProxyBaseAppContext"

  def _check_properties(self):
    global last_context
    global last_context_time
    if last_context_time is None or last_context_time + _STALE_CONTEXT_DELTA < time.time():
      last_context = communication.server.get_context()
      last_context_time = time.time()
    properties = last_context
    matches = {}
    for (key, value) in self.arguments.iteritems():
      matches[key] = False
      if (key in properties and
          self._property_match(key, properties[key], self.arguments[key])):
        matches[key] = True
    return matches

  def _property_match(self, key, actual, desired):
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

class ProxyAppRegexContext(ProxyBaseAppContext):
  def _property_match(self, key, actual, desired):
    return bool(re.match(desired, actual))

__all__ = ["ProxyAppContextAnd", "ProxyAppContextOr", "ProxyAppContext", "ProxyAppRegexContext"]
