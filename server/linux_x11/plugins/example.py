from yapsy.IPlugin import IPlugin

enabled = True


def greet_user(name='Incognito'):
    '''RPC command to greet a user. See the _server_plugin_example grammar for
       how to use on the client side via voice.'''
    print 'Hello user %s!' % name


class ExamplePlugin(IPlugin):
    def register_rpcs(self, server):
        server.register_function(greet_user)
