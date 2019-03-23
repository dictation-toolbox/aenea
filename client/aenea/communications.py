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

import httplib
import jsonrpclib
import socket
import time

import aenea.config
import aenea.configuration

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


class _ImpatientTransport(jsonrpclib.jsonrpc.Transport):
    '''Transport for jsonrpclib that supports a timeout.'''
    def __init__(self, timeout=None):
        self._timeout = timeout
        jsonrpclib.jsonrpc.Transport.__init__(self)

    def make_connection(self, host):
        #return an existing connection if possible.  This allows
        #HTTP/1.1 keep-alive.
        if (hasattr(self, '_connection') and
            self._connection is not None and
            host == self._connection[0]):
            return self._connection[1]

        # create a HTTP connection object from a host descriptor
        chost, self._extra_headers, x509 = self.get_host_info(host)
        #store the host argument along with the connection object
        if self._timeout is None:
            self._connection = host, httplib.HTTPConnection(chost)
        else:
            self._connection = host, httplib.HTTPConnection(
                chost,
                timeout=self._timeout
                )
        return self._connection[1]


def _adjust_arguments(a, kw, security_token):
    # Cannot use both positional and keyword arguments
    # (according to JSON-RPC spec.)
    assert not (a and kw)

    if security_token is not None:
        if a:
            a = a + (security_token,)
        else:
            kw = kw.copy()
            kw['security_token'] = security_token

    return a, kw


class Proxy(object):
    def __init__(self):
        self._address = None
        self.last_connect_good = False
        self._last_failed_connect = 0
        self._transport = _ImpatientTransport(aenea.config.COMMAND_TIMEOUT)
        self._security_token = getattr(aenea.config, 'SECURITY_TOKEN', None)

    def _execute_batch(self, batch, use_multiple_actions=False):
        self._refresh_server()
        if self._address is None:
            return
        if time.time() - self._last_failed_connect > aenea.config.CONNECT_RETRY_COOLDOWN:
            try:
                if not self.last_connect_good:
                    socket.create_connection(self._address, aenea.config.CONNECT_TIMEOUT)
                self.last_connect_good = True

                if len(batch) == 1:
                    return (getattr(
                        self._server,
                        batch[0][0])(*batch[0][1], **batch[0][2])
                        )
                elif use_multiple_actions:
                    if self._security_token is not None:
                        self._server.multiple_actions(actions=batch, security_token=self._security_token)
                    else:
                        self._server.multiple_actions(actions=batch)
                else:
                    for (command, args, kwargs) in batch:
                        getattr(self._server, command)(*args, **kwargs)
            except socket.error:
                self._last_failed_connect = time.time()
                self.last_connect_good = False
                print 'Socket error connecting to aenea server. To avoid slowing dictation, we won\'t try again for %i seconds.' % aenea.config.CONNECT_RETRY_COOLDOWN

    def execute_batch(self, batch):
        self._execute_batch(batch, aenea.config.USE_MULTIPLE_ACTIONS)

    def __getattr__(self, meth):
        def call(*a, **kw):
            a, kw = _adjust_arguments(a, kw, self._security_token)

            return self._execute_batch([(meth, a, kw)])
        return call

    def _refresh_server(self):
        _server_config.refresh()
        address = _server_config.conf['host'], _server_config.conf['port']
        if self._address != address:
            self.last_connect_good = False
            self._address = address
            self._server = jsonrpclib.Server(
                'http://%s:%i' % address,
                transport=self._transport
                )
            self._last_failed_connect = 0


class BatchProxy(object):
    def __init__(self):
        self._security_token = getattr(aenea.config, 'SECURITY_TOKEN', None)
        self._commands = []

    def __getattr__(self, key):
        def call(*a, **kw):
            a, kw = _adjust_arguments(a, kw, self._security_token)

            if not key.startswith('_'):
                self._commands.append((key, a, kw))
        return call

server = Proxy()
