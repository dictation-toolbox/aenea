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

# Match only if no value set
VALUE_NOT_SET = object()

# Match only if value set but any value
VALUE_SET = object()

# Do not consider this argument.
VALUE_DONT_CARE = object()

# Hate to do this, but currently we hit the server once per context,
# and the get_context() call is actually rather expensive. Ideally we
# could propagate state through Dragonfly contexts, but that would involve
# deep surgery or re-implementation.
_last_context = None
_last_context_time = 0
_STALE_CONTEXT_DELTA = 0.01

def get_context():
  global _last_context
  global _last_context_time
  if (_last_context_time is None or
      _last_context_time + _STALE_CONTEXT_DELTA < time.time()):
    _last_context = communication.server.get_context()
    _last_context_time = time.time()
  return _last_context

class AlwaysContext(dragonfly.Context):
  def matches(self, windows_executable, windows_title, windows_handle):
    return True

class NeverContext(dragonfly.Context):
  def matches(self, windows_executable, windows_title, windows_handle):
    return False

class ProxyCustomAppContext(dragonfly.Context):
  """matches based on the properties of the currently active window.
     Match may be "substring", "exact", or "regex". logic may be "and",
     "or" or an integer (to match if at least N clauses satisfied.)"""
  def __init__(self, match="substring", logic="and", case_sensitive=False,
               query=None, **kw):
    if query is None:
      query = {}
    query.update(kw)
    self._str = "ProxyBaseAppContext"
    self.match = match
    self.logic = logic
    self.case_sensitive = case_sensitive
    self.arguments = query
    dragonfly.Context.__init__(self)

    assert match in ("exact", "substring", "regex")
    if logic not in ("and", "or"):
      assert int(logic) >= 0 and int(logic) <= len(query)

  def _check_properties(self):
    properties = get_context()
    matches = {}
    for (key, value) in self.arguments.iteritems():
      if value == VALUE_DONT_CARE:
        continue
      matches[key] = False
      if value == VALUE_NOT_SET:
        matches[key] = (key not in properties)
      elif value == VALUE_SET:
        matches[key] = (key in properties)
      elif key in properties:
        matches[key] = self._property_match(key, properties[key],
                                            self.arguments[key])
    return matches

  def _property_match(self, key, actual, desired):
    """overload to change how we should compare actual and desired properties"""
    if not self.case_sensitive:
      actual = actual.lower()
      desired = desired.lower()
    if self.match == "substring":
      return desired in actual
    elif self.match == "exact":
      return desired == actual
    else:
      return bool(re.match(desired, actual))

  def _reduce_matches(self, matches):
    """overload to change the logic that should be used to combine the results
       of the matching function"""
    if self.logic == "and":
      return all(matches.itervalues())
    elif self.logic == "or":
      return any(matches.itervalues())
    else:
      return len(filter(None, matches.itervalues())) >= int(self.logic)

  def matches(self, windows_executable, windows_title, windows_handle):
    return self._reduce_matches(self._check_properties())

def ProxyAppContext(
    title=VALUE_DONT_CARE,
    cls=VALUE_DONT_CARE,
    cls_name=VALUE_DONT_CARE,
    executable=VALUE_DONT_CARE,
    match="substring",
    logic="and",
    case_sensitive=False):
  """tries to do the right thing depending on the server on the other end.
     prefer using this when possible, as cls and cls_name will be automatically
     dropped on platforms that do not define them."""
  properties = get_context()

  query = {
      "title":title,
      "cls":cls, 
      "cls_name":cls_name,
      "executable":executable
    }

  if "cls" not in properties or "cls_name" not in properties:
    del query["cls"]
    del query["cls_name"]

  return ProxyCustomAppContext(match=match, logic=logic, query=query)

__all__ = ["ProxyAppContext", "ProxyCustomAppContext", "AlwaysContext", "NeverContext", "VALUE_NOT_SET", "VALUE_SET", "VALUE_DONT_CARE"]
