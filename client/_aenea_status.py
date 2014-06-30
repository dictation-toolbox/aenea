try:
    import aenea.config
except ImportError:
    print 'Aenea: Unable to import config file. Maybe you need to copy config.py.example to config.py in C:\NatLink\NatLink\MacroSystem? (or in your Aenea folder if using Gladstone)'
    raise

try:
    import aenea.communications
except ImportError:
    print "Error importing communications module."
    raise

try:
    import aenea.proxy_actions
    import aenea.proxy_contexts
    import aenea.proxy_nicknames
except ImportError:
    print "Aenea: Error importing proxy modules. This most commonly occurs if we cannot connect to the server."
    raise

print 'Aenea client-side modules loaded successfully'
print 'Settings:'
print '\tHOST:', aenea.config.HOST
print '\tPORT:', aenea.config.PORT
print '\tPLATFORM:', aenea.config.PLATFORM
print '\tUSE_MULTIPLE_ACTIONS:', aenea.config.USE_MULTIPLE_ACTIONS
print '\tSCREEN_RESOLUTION:', aenea.config.SCREEN_RESOLUTION

try:
    aenea.proxy_contexts.get_context()
    print 'Aenea: Successfully connected to server.'
except:
    print 'Aenea: Unable to connect to server. Make sure the server is running on the host and restart Dragon.'
