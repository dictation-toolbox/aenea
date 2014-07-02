import jsonrpclib
import socket
import time

import aenea.config

_last_failed_connect = 0


class Proxy(object):
    def __init__(self):
        self._address = None

    def execute_batch(self, batch):
        global _last_failed_connect
        self._refresh_server()
        if time.time() - _last_failed_connect > aenea.config.CONNECT_RETRY_COOLDOWN:
            try:
                if aenea.config.USE_MULTIPLE_ACTIONS and len(batch) > 1:
                    self._server.multiple_actions(batch)
                else:
                    for (command, args, kwargs) in batch:
                        getattr(self._server, command)(*args, **kwargs)
            except socket.error:
                _last_failed_connect = time.time()
                print 'Failed to connect to aenea server. To avoid slowing dictation, we won\'t try again for %i seconds.' % aenea.config.CONNECT_RETRY_COOLDOWN

    def __getattr__(self, meth):
        self._refresh_server()
        def call(*a, **kw):
            global _last_failed_connect
            if time.time() - _last_failed_connect > aenea.config.CONNECT_RETRY_COOLDOWN:
                try:
                    return getattr(self._server, meth)(*a, **kw)
                except socket.error:
                    _last_failed_connect = time.time()
                    print 'Failed to connect to aenea server. To avoid slowing dictation, we won\'t try again for %i seconds.' % aenea.config.CONNECT_RETRY_COOLDOWN
        return call

    def _refresh_server(self):
        conf = aenea.config.get_server_address()
        if self._address != conf:
            self._address = conf
            self._server = jsonrpclib.Server('http://%s:%i' % conf)


class BatchProxy(object):
    def __init__(self):
        self._commands = []

    def __getattr__(self, key):
        def call(*a, **kw):
            if not key.startswith('_'):
                self._commands.append((key, a, kw))
        return call

server = Proxy()
