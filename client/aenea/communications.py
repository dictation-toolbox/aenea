# This file is part of Aenea
#
# Aenea is free software: you can redistribute it and/or modify it under
# the terms of version 3 of the GNU Lesser General Public License as
# published by the Free Software Foundation.
#
# Aenea is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with Aenea.  If not, see <http://www.gnu.org/licenses/>.
#
# Copyright (2014) Alex Roper
# Alex Roper <alex@aroper.net>

import jsonrpclib
import socket
import time

import aenea.config
import aenea.configuration

_last_failed_connect = 0


_server_config = aenea.configuration.ConfigWatcher(
    'server_state',
    {'host': aenea.config.DEFAULT_SERVER_ADDRESS[0],
     'port': aenea.config.DEFAULT_SERVER_ADDRESS[1]})
_server_config.write()


def set_server_address(address):
    '''address is (host, port).'''
    _server_config.refresh()
    _server_config['host'], _server_config['port'] = address
    _server_config.write()

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
        def call(*a, **kw):
            global _last_failed_connect
            self._refresh_server()
            if time.time() - _last_failed_connect > aenea.config.CONNECT_RETRY_COOLDOWN:
                try:
                    return getattr(self._server, meth)(*a, **kw)
                except socket.error:
                    _last_failed_connect = time.time()
                    print 'Failed to connect to aenea server. To avoid slowing dictation, we won\'t try again for %i seconds.' % aenea.config.CONNECT_RETRY_COOLDOWN
        return call

    def _refresh_server(self):
        _server_config.refresh()
        address = _server_config.conf['host'], _server_config.conf['port']
        if self._address != address:
            self._address = address
            self._server = jsonrpclib.Server('http://%s:%i' % address)


class BatchProxy(object):
    def __init__(self):
        self._commands = []

    def __getattr__(self, key):
        def call(*a, **kw):
            if not key.startswith('_'):
                self._commands.append((key, a, kw))
        return call

server = Proxy()
