import jsonrpclib

import config

class Proxy(object):
  def __init__(self, host, port):
    self.server = jsonrpclib.Server("http://%s:%i" % (host, port))

  def execute_batch(self, batch):
    if config.USE_MULTIPLE_ACTIONS:
      self.server.multiple_actions(batch)
    else:
      for (command, args, kwargs) in batch:
        getattr(self.server, command)(*args, **kwargs)

class BatchProxy(object):
  def __init__(self):
    self._commands = []

  def __getattr__(self, key):
    def call(*a, **kw):
      if not key.startswith("_"):
        self._commands.append((key, a, kw))
    return call
