# This is a command module for Dragonfly. It provides support for several of
# Aenea's built-in capabilities. This module is NOT required for Aenea to
# work correctly, but it is strongly recommended.

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

import os
import sys

import dragonfly

try:
    # Internal NatLink module for reloading grammars.
    import natlinkmain
except ImportError:
    natlinkmain = None

try:
    import aenea
    import aenea.proxy_contexts
    import aenea.configuration
    import aenea.communications
    import aenea.config
    import aenea.configuration
except ImportError:
    print 'Unable to import Aenea client-side modules.'
    raise

print 'Aenea client-side modules loaded successfully'
print 'Settings:'
print '\tHOST:', aenea.config.DEFAULT_SERVER_ADDRESS[0]
print '\tPORT:', aenea.config.DEFAULT_SERVER_ADDRESS[1]
print '\tPLATFORM:', aenea.config.PLATFORM
print '\tUSE_MULTIPLE_ACTIONS:', aenea.config.USE_MULTIPLE_ACTIONS
print '\tSCREEN_RESOLUTION:', aenea.config.SCREEN_RESOLUTION

try:
    aenea.proxy_contexts._get_context()
    print 'Aenea: Successfully connected to server.'
except:
    print 'Aenea: Unable to connect to server.'


# Commands that can be rebound.
command_table = [
    'set proxy server to <proxy>',
    'disable proxy server',
    'enable proxy server',
    'force natlink to reload all grammars'
    ]
command_table = aenea.configuration.make_grammar_commands(
    'aenea',
    dict(zip(command_table, command_table))
    )


def topy(path):
    if path.endswith == ".pyc":
        return path[:-1]

    return path


class DisableRule(dragonfly.CompoundRule):
    spec = command_table['disable proxy server']

    def _process_recognition(self, node, extras):
        aenea.config.disable_proxy()


class EnableRule(dragonfly.CompoundRule):
    spec = command_table['enable proxy server']

    def _process_recognition(self, node, extras):
        aenea.config.enable_proxy()


def reload_code():
    # Do not reload anything in these directories or their subdirectories.
    dir_reload_blacklist = set(["core"])
    macro_dir = "C:\\NatLink\\NatLink\\MacroSystem"

    # Unload all grammars if natlinkmain is available.
    if natlinkmain:
        natlinkmain.unloadEverything()

    # Unload all modules in macro_dir except for those in directories on the
    # blacklist.
    # Consider them in sorted order to try to make things as predictable as possible to ease debugging.
    for name, module in sorted(sys.modules.items()):
        if module and hasattr(module, "__file__"):
            # Some builtin modules only have a name so module is None or
            # do not have a __file__ attribute.  We skip these.
            path = module.__file__

            # Convert .pyc paths to .py paths.
            path = topy(path)

            # Do not unimport this module!  This will cause major problems!
            if (path.startswith(macro_dir) and
                not bool(set(path.split(os.path.sep)) & dir_reload_blacklist)
                and path != topy(os.path.abspath(__file__))):

                print "removing %s from cache" % name

                # Remove the module from the cache so that it will be reloaded
                # the next time # that it is imported.  The paths for packages
                # end with __init__.pyc so this # takes care of them as well.
                del sys.modules[name]

    try:
        # Reload the top-level modules in macro_dir if natlinkmain is available.
        if natlinkmain:
            natlinkmain.findAndLoadFiles()
    except Exception as e:
        print "reloading failed: {}".format(e)
    else:
        print "finished reloading"


# Note that you do not need to turn mic off and then on after saying this.  This
# also unloads all modules and packages in the macro directory so that they will
# be reloaded the next time that they are imported.  It even reloads Aenea!
class ReloadGrammarsRule(dragonfly.MappingRule):
    mapping = {command_table['force natlink to reload all grammars']: dragonfly.Function(reload_code)}

server_list = dragonfly.DictList('aenea servers')
server_list_watcher = aenea.configuration.ConfigWatcher(
    ('grammar_config', 'aenea'))


class ChangeServer(dragonfly.CompoundRule):
    spec = command_table['set proxy server to <proxy>']
    extras = [dragonfly.DictListRef('proxy', server_list)]

    def _process_recognition(self, node, extras):
        aenea.communications.set_server_address((extras['proxy']['host'], extras['proxy']['port']))

    def _process_begin(self):
        if server_list_watcher.refresh():
            server_list.clear()
            for k, v in server_list_watcher.conf.get('servers', {}).iteritems():
                server_list[str(k)] = v

grammar = dragonfly.Grammar('aenea')

grammar.add_rule(EnableRule())
grammar.add_rule(DisableRule())
grammar.add_rule(ReloadGrammarsRule())
grammar.add_rule(ChangeServer())

grammar.load()


# Unload function which will be called at unload time.
def unload():
    global grammar
    if grammar:
        grammar.unload()
    grammar = None
