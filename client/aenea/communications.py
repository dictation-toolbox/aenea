import jsonrpclib
import socket
import time

import aenea.config

_last_failed_connect = 0


class Proxy(object):
    def __init__(self, host, port):
        self._server = jsonrpclib.Server('http://%s:%i' % (host, port))

    def execute_batch(self, batch):
        global _last_failed_connect
        if time.time() - _last_failed_connect > aenea.config.CONNECT_RETRY_COOLDOWN:
            try:
                if aenea.config.USE_MULTIPLE_ACTIONS and len(batch) > 1:
                    self._server.multiple_actions(batch)
                else:
                    for (command, args, kwargs) in batch:
                        getattr(self._server, command)(*args, **kwargs)
            except socket.error as e:
                _last_failed_connect = time.time()
                print 'Failed to connect to aenea server. To avoid slowing dictation, we won\'t try again for %i seconds.' % aenea.config.CONNECT_RETRY_COOLDOWN

    def __getattr__(self, meth):
        def call(*a, **kw):
            return self.execute_batch([(meth, a, kw)])
        return call


class BatchProxy(object):
    def __init__(self):
        self._commands = []

    def __getattr__(self, key):
        def call(*a, **kw):
            if not key.startswith('_'):
                self._commands.append((key, a, kw))
        return call

server = Proxy(aenea.config.HOST, aenea.config.PORT)
