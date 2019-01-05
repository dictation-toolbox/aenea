# Dragonfly module for controlling the Linux terminal
# The context of this file will be strictly for terminal use

from aenea import *
import terminator

terminal_context = aenea.wrappers.AeneaContext(
    ProxyAppContext(match='regex', title='(?i).*simon@simon.*'), # app_id='Terminal'
    AppContext(executable='Terminal')
    )

class TerminalRule(MappingRule):
    mapping = {
            ## Global Commands

            # Open folder
            'open folder': Text('nautilus .') + Key('enter'),
            # The pseudo command
            'master': Text('sudo '),
            # Managing folders and files.
            'folders': Text('ls') + Key('enter'),
            # Add a folder
            'Add folder': Text('mkdir '),
            'navigate': Text('cd '),
            'historian': Key('c-r'),
            # Managing terminal windows.
            'Tony [<n>]': Key('a-%(n)s'),
            'Exeter': Key('c-d'),
            'newer': Key('c-t'),
            'clearer': Key('c-l'),
            'cleaner': Key('c-u'),
            # Clearing and cancelling actions.
            'squash': Key('ca-c'),
            'crush': Key('cs-d'),
            # Killing all programs on a port
            'desiccate': Text('kill -9 $(lsof -ti :)'),
            # Killing all programs on a port
            'reveal': Text('lsof -i :'),
            # Scroll up in the terminal
            'decline': Key('s-pgdown'),
            # Scroll down in the terminal
            'incline': Key('s-pgup')
        }
    extras = [
        Dictation("text"),
        IntegerRef("n", 1, 100),
    ]
    defaults = {
        "n": 1,
    }

gitcommand_array = [
    'add',
    'branch',
    'checkout',
    'clone',
    'commit',
    'diff',
    'fetch',
    'init',
    'log',
    'merge',
    'pull',
    'push',
    'rebase',
    'reset',
    'show',
    'stash',
    'status',
    'tag',
]
gitcommand = {}
for command in gitcommand_array:
    gitcommand[command] = command

class GitRule(MappingRule):
    mapping = {
        # Git
        "(git|get)": Text("git "),
        "(git|get) <gitcommand>": Text("git %(gitcommand)s "),
    }
    extras = [
        Dictation("text"),
        IntegerRef("n", 1, 100),
        Choice('gitcommand', gitcommand),
    ]
    defaults = {
        "n": 1,
    }

class ApplicationRule(MappingRule):
    mapping = {
            # Basic root applications.
            'post': Text('psql '),
            'really': Text('rails '),
            'doctor': Text('docker '),
            'doctor compose': Text('docker-compose '),
            'subtle': Text('subl .') + Key('enter'),
            # Rails commands
            'bundle install': Text('bundle install'),
            'bundle update': Text('bundle update'),
            'bundle exec': Text('bundle exec '),
            'really my':  Text('rails db:migrate'),
        }
    extras = [
        Dictation("text"),
        IntegerRef("n", 1, 100),
    ]
    defaults = {
        "n": 1,
    }

class ShiftRule(MappingRule):
    mapping = {
            # Launching the application

            'shifty boot': Text('./bin/dev/boot') + Key('enter'),
            'shifty yawn': Text('./bin/dev/yarn '),
            'shifty post': Text('psql -p 5432 -h localhost -U postgres'),
            'shifty console': Text('./bin/dev/console') + Key('enter'),
            'shifty test': Text('./bin/dev/test '),
            'shifty rails': Text('./bin/dev/rails '),
            'shifty bundle': Text('./bin/dev/bundle '),
            'shifty break': Text('./bin/dev/rake '),
            'consultant (<text>)': Text("Tenant.switch('%(text)s') {binding.pry}")
        }
    extras = [
        Dictation("text"),
        IntegerRef("n", 1, 100),
    ]
    defaults = {
        "n": 1,
    }

class NodeYarnRule(MappingRule):
    mapping = {
            # Launching the application

            '(your | John) install': Text('yarn install'),
            '(your | John) add': Text('yarn add ')
        }
    extras = [
        Dictation("text"),
        IntegerRef("n", 1, 100),
    ]
    defaults = {
        "n": 1,
    }


phoenixcommand = {
    "new": ".new",
    "server": ".server",
    "Jane context": ".gen.context",
    "Jane html": ".gen.html",
}

mixedcommand = {
    "depth get": "deps.get",
    "ecto setup": "ecto.setup",
}

class ElixirRule(MappingRule):
    mapping = {
            ## Commands
            'eggs': Text("elixir "),
            'mix <mixedcommand>': Text("mix %(mixedcommand)s"),
            'ecto': Text("ecto"),
            'Ickes': Text("iex "),
            'phoenix <phoenixcommand>': Text("phx%(phoenixcommand)s"),
            ## File affixes
            'ex file': Text('.ex'),
            'ex script': Text('.exs'),
        }
    extras = [
        Dictation("text"),
        IntegerRef("n", 1, 100),
        Choice('phoenixcommand', phoenixcommand),
        Choice('mixedcommand', mixedcommand),
    ]
    defaults = {
        "n": 1,
    }

grammar = Grammar("terminal", context=terminal_context) # Create grammar
grammar.add_rule(TerminalRule())  # Add the top-level rule.
grammar.add_rule(GitRule())  # At the git rule
grammar.add_rule(ApplicationRule())  # Add the top-level rule.
grammar.add_rule(ShiftRule())  # Add the shift rule.
grammar.add_rule(NodeYarnRule())  # Add the NodeYarn rule.
grammar.add_rule(ElixirRule())  # Add the NodeYarn rule.
grammar.add_rule(terminator.TerminatorRule()) # Add the terminator rule
grammar.load()  # Load the grammar.

def unload():
    """Unload function which will be called at unload time."""
    global grammar
    if grammar:
        grammar.unload()
    grammar = None
