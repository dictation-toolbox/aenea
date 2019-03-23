import abc
import time
import logging
import logging.config

from jsonrpclib.SimpleJSONRPCServer import SimpleJSONRPCServer


class PermissionDeniedError(Exception):
    pass


class AeneaServer(object):
    """
    AeneaServer is a jsonrpc server that exposes emulated keyboard/mouse input
    over the network.  Takes care of the JSON RPC protocol that Aenea is built
    on top of and handles dispatching RPCs to the appropriate action.
    """
    def __init__(self, rpc_impl, server, plugins=tuple(), logger=None, security_token=None):
        """
        :param rpc_impl: Object that implements all AbstractAeneaPlatformRpc
         methods.  This is where the platform specific magic happens to gather
         context and emulate input.
        :param SimpleJSONRPCServer server: rpcs from <rpc_impl> will be attached
          to this server.
        :param plugins: yapsy plugin objects.  Plugin objects are required to
          implement a single method "register_rpcs(server)" where server is
          an instance of SimpleJSONRPCServer.  It is up to the plugin developer
          to register new RPCs in this method with a call to
          "server.register_function(...)"
        :param logger:
        :param security_token: Static token that must be the same between
         client and server.
        """
        self.logger = logger or logging.getLogger(self.__class__.__name__)
        self.server = server
        self.security_token = None

        self.logger.debug('using {0} for input emulation'.format(
            rpc_impl.__class__.__name__))

        for rpc_func, rpc_name in rpc_impl.rpc_commands.items():
            self.server.register_function(rpc_name, rpc_func)
        self.server.register_function(self.multiple_actions, 'multiple_actions')

        for plugin in plugins:
            plugin.register_rpcs(self.server)

        self.rpc_impl = rpc_impl

    @classmethod
    def from_config(cls, platform_rpcs, config):
        """
        Create an AeneaServer instance from config
        :param platform_rpcs: Concrete instance of AbstractAeneaPlatformRpc
        :param config: Aenea configuration parameters.  This is generally
         Aenea's config.py module.
        :return: AeneaServer instance
        :rtype: AeneaServer
        """
        AeneaLoggingManager.configure(
                level=getattr(config, 'LOG_LEVEL', None),
                log_file=getattr(config, 'LOG_FILE', None))
        logger = logging.getLogger(AeneaLoggingManager.aenea_logger_name)

        rpc_server = SimpleJSONRPCServer(
                (config.HOST, config.PORT), logRequests=False)

        security_token = getattr(config, 'SECURITY_TOKEN', None)

        # TODO: dynamically load/instantiate platform_rpcs from config instead
        # of requiring it as an explicit argument

        plugins = AeneaPluginLoader(logger).get_plugins(
                getattr(config, 'PLUGIN_PATH', None))

        return cls(platform_rpcs, rpc_server, plugins=plugins, logger=logger, security_token=security_token)

    def serve_forever(self):
        self.logger.debug(
            'starting server on {0}:{1}'.format(*self.server.server_address))
        self.server.serve_forever()

    def multiple_actions(self, actions, security_token=None):
        """
        Execute multiple rpc commands, aborting on any error. Guaranteed to
        execute in specified order. See also JSON-RPC multicall. Annoyingly, we
        currently need to check the security token both for the multicall and
        in the RPCs, so it must be specified for both.
        :param list actions:  List of dicts.  Each dictionary must provide
          "method", "optional", and "parameters" keys. e.g.
          ..code python
            {
                'method': 'key_press',
                'parameters': None,
                'optional': {'key': 'a', 'direction': 'press', 'count': 2}
            }
        :param security_token: Static token that must be the same between
         client and server.
        :return: This function always returns None
        :rtype: None
        """
        self.rpc_impl._check_security_token(security_token)

        for (method, parameters, optional) in actions:
            if method in self.server.funcs:
                # JSON-RPC forbids specifying both optional and parameters.
                # Since multiple_actions is trying to mimic something like
                # Multicall except with sequential ordering and abort,
                # we enforce it here.
                assert not (parameters and optional)
                self.server.funcs[method](*parameters, **optional)
            else:
                break


def compare_security_token(expected, actual):
    if len(expected) != len(actual):
        return False
    result = 0
    for x, y in zip(expected, actual):
        result |= ord(x) ^ ord(y)
    return result == 0


class AbstractAeneaPlatformRpcs(object):
    """
    Interface that defines Aenea's supported RPCs.  This is where the platform
    specific magic happens for each of Aenea's server distros. Concrete
    subclasses must provide implementations for server_info, get_context,
    and rpc_commands.
    """

    __metaclass__ = abc.ABCMeta

    def __init__(self, logger=None, security_token=None):
        self.logger = logger or logging.getLogger(self.__class__.__name__)
        self.security_token = security_token

    def _check_security_token(self, security_token):
        if self.security_token is None:
            self.logger.warn('Server is configured to disable checking security tokens. You can use generate_security_token.py to generate a security token, which you should then add to config.py (client) and aenea.json (server). This message is intentionally spammy and annoying -- you need to fix this.')
            return

        if security_token is None:
            error_text = 'Client did not send a security token, but server has security token set. To fix, find the client\'s aenea.json and add security_token: "foo", to it, then restart Dragon. You will need to replace foo with the server\'s security token, which you can find in config.py. Or generate a new random one with generate_security_token.py and set it in both client and server.'
            self.logger.error(error_text)
            raise PermissionDeniedError(error_text)
        elif not compare_security_token(self.security_token, security_token):
            error_text = 'Client sent a security token, but it did not match the server\'s. The server\'s is specified in config.py. The client\'s is specified in aenea.json. Use generate_security_token.py to create a random token.'
            self.logger.error(error_text)
            raise PermissionDeniedError(error_text)
        else:
            pass


    @property
    def rpc_commands(self):
        """
        Returns a dict of RPCs to be exposed to clients.
        :return: dict of {<rpc_name>: <rpc_callable>}
        :rtype: dict
        """
        return {
            'server_info': self.server_info,
            'get_context': self.get_context,
            'key_press': self.key_press,
            'write_text': self.write_text,
            'click_mouse': self.click_mouse,
            'move_mouse': self.move_mouse,
            'pause': self.pause,
            'notify': self.notify,
        }

    @abc.abstractmethod
    def server_info(self, security_token=None):
        """
        Return arbitrary server information to the aenea client.
        :param security_token: Static token that must be the same between
         client and server.
        :return:
        :rtype: dict
        """
        self._check_security_token(security_token)
        raise NotImplementedError()

    @abc.abstractmethod
    def get_context(self, security_token=None):
        """
        Query the system for context information.  This data is typically passed
        back to the aenea client so that it may use it in Dragonfly grammars.
        Specifically, this data will be used when Dragonfly's grammars perform
        context matching to decide which grammars should be activated.
        :param security_token: Static token that must be the same between
         client and server.
        :return: various properties related to the current active window
        """
        self._check_security_token(security_token)
        raise NotImplementedError()

    def key_press(self, key=None, modifiers=(), direction='press', count=1,
                  count_delay=None, security_token=None):
        """
        Press a key possibly modified by modifiers. direction may be 'press',
        'down', or 'up'. modifiers may contain 'alt', 'shift', 'control',
        'super'.
        :param str key: Key to press.
        :param modifiers: Key modifiers.
        :type modifiers: list of str.
        :param str direction: Direction of key press.
        :param int count: Number of times to perform this key press.
        :param int count_delay: Delay between repeated keystrokes in
         milliseconds.
        :param security_token: Static token that must be the same between
         client and server.
        :return: This function always returns None
        """
        self._check_security_token(security_token)
        raise NotImplementedError()

    def write_text(self, text, security_token=None):
        """
        Send text formatted exactly as written to active window.
        :param str text: Text to send to the current active window.
        :param security_token: Static token that must be the same between
         client and server.
        :return: This function always returns None
        """
        self._check_security_token(security_token)
        raise NotImplementedError()

    def click_mouse(self, button, direction='click', count=1, count_delay=None, security_token=None):
        """
        Click the mouse button specified at the current location.
        :param button: Mouse button to click.
        :type button: str or int
        :param str direction: Direction of 'up', 'down', 'click'
        :param int count: Number of times to repeat this click.
        :param int count_delay: Delay in milliseconds between mouse clicks.
        :param security_token: Static token that must be the same between
         client and server.
        :return: This function always returns None
        """
        self._check_security_token(security_token)
        raise NotImplementedError()

    def move_mouse(self, x, y, reference='absolute', proportional=False,
                   phantom=None, security_token=None):
        """
        Move the mouse to the specified coordinates.
        :param x: x coordinate for move
        :param y: y coordinate for move
        :param str reference: One of 'absolute', 'relative' or
         'relative_active':

          - absolute: Move the mouse to the absolute position x, y.  x and y
           will be set to 0 if they are negative.
          - relative: Move the mouse relative to its current location. x and y
           may be negative integers.
          - relative_active: Move the mouse relative to the current active
           window. 0,0 being the top left corner of with window.

        :param proportional:
        :param phantom: If provided, move to the desired location, click the
         <phantom> button and restore the mouse to the original location.
        :param security_token: Static token that must be the same between
         client and server.
        :type phantom: str or None
        :return: This function always returns None.
        """
        self._check_security_token(security_token)
        raise NotImplementedError()

    def pause(self, amount, security_token=None):
        """
        Pause command execution.
        :param int amount: number of milliseconds to sleep for.
        :param security_token: Static token that must be the same between
         client and server.
        :return: This function always returns None.
        """
        self._check_security_token(security_token)
        # we can get away with a concrete impl here because python provides
        # cross platform sleep.
        time.sleep(amount / 1000.0)

    def notify(self, message, security_token=None):
        """
        Send a message to the desktop to be displayed in a notification window.
        :param str message: message to send to the desktop.
        :param security_token: Static token that must be the same between
         client and server.
        :return: This function always returns None.
        """
        self._check_security_token(security_token)
        raise NotImplementedError()


class AeneaPluginLoader(object):
    """
    Manages loading Aenea server plugins.
    """
    def __init__(self, logger=None):
        self.logger = logger or logging.getLogger(self.__class__.__name__)

    def get_plugins(self, plugin_path=None):
        """
        Get plugins from a given location.
        :param plugin_path: Path to directory containing yapsy Aenea server
         plugins.
        :return: All plugins loaded found in plugin_path
        :rtype: List of yapsy plugins
        """
        if plugin_path is None:
            return []

        try:
            import yapsy
            import yapsy.PluginManager
        except ImportError:
            self.logger.warn(
                'Cannot import yapsy; optional server plugin support won\'t '
                'work. You don\'t need this unless you want to use plugins, '
                'which are not necessary for basic operation.')
            return []

        plugin_manager = yapsy.PluginManager.PluginManager()
        plugin_manager.setPluginPlaces(plugin_path)
        plugin_manager.collectPlugins()

        plugins = []
        for plugin_info in plugin_manager.getAllPlugins():
            self.logger.debug('Loading plugin "%s"' % plugin_info.name)
            plugin_manager.activatePluginByName(plugin_info.name)
            plugin = plugin_info.plugin_object

            # TODO: duck type checking for a callable register_rpcs attribute on
            # plugin should go here.

            plugins.append(plugin)

        return plugins


class AeneaLoggingManager(object):
    """
    Handles generating and configuring basic logging support for Aenea
    """
    aenea_logger_name = 'aenea'

    default_config = {
        'version': 1,
        'formatters': {
            'generic': {
                'format': '%(asctime)s [%(levelname)-6s] [%(name)-s] %(message)s'
            }
        },
        'handlers': {
            'console': {
                'class': 'logging.StreamHandler',
                'level': 'DEBUG',
                'formatter': 'generic'
            }
        },
        'loggers': {
            'root': {
                'level': 'DEBUG',
                'handlers': [],
                'propagate': True
            },
            aenea_logger_name: {
                'level': 'DEBUG',
                'handlers': ['console']
            }
        }
    }

    @classmethod
    def configure(cls, level=None, log_file=None):
        """
        Configure logging.
        :param str level: python logging level. e.g. 'INFO' | 'DEBUG'
        :param str log_file: location of log file to write to..
        :return: None
        :type: None
        """
        level = level or 'DEBUG'
        config = cls.default_config.copy()
        config['handlers']['console']['level'] = level

        if log_file is not None:
            config['handlers']['file'] = {
                'class': 'logging.handlers.RotatingFileHandler',
                'formatter': 'generic',
                'level': level,
                'filename': log_file,
                'mode': 'a',
                'backupCount': 3
            }
            config['loggers'][cls.aenea_logger_name]['handlers'].append('file')

        logging.config.dictConfig(config)
