import aenea.proxy_actions
import aenea.proxy_contexts

try:
    import dragonfly
except ImportError:
    import dragonfly_mock as dragonfly


# Dragonfly builtins
Grammar = dragonfly.Grammar
ConnectionGrammar = dragonfly.ConnectionGrammar

Rule = dragonfly.Rule
CompoundRule = dragonfly.CompoundRule
MappingRule = dragonfly.MappingRule

ElementBase = dragonfly.ElementBase
Sequence = dragonfly.Sequence
Alternative = dragonfly.Alternative
Optional = dragonfly.Optional
Repetition = dragonfly.Repetition
Literal = dragonfly.Literal
RuleRef = dragonfly.RuleRef
ListRef = dragonfly.ListRef
IntegerRef = dragonfly.IntegerRef
Integer = dragonfly.Integer
Dictation = dragonfly.Dictation
DictListRef = dragonfly.DictListRef
Compound = dragonfly.Compound
Choice = dragonfly.Choice

ActionBase = dragonfly.ActionBase
Repeat = dragonfly.Repeat
Key = dragonfly.Key
Text = dragonfly.Text
Paste = dragonfly.Paste
Mouse = dragonfly.Mouse
Function = dragonfly.Function
Mimic = dragonfly.Mimic
Playback = dragonfly.Playback
WaitWindow = dragonfly.WaitWindow
FocusWindow = dragonfly.FocusWindow
Pause = dragonfly.Pause

Context = dragonfly.Context
AppContext = dragonfly.AppContext

# Redefine Aenea objects that shadow.
Key = aenea.proxy_actions.ProxyKey
Text = aenea.proxy_actions.ProxyText
Mouse = aenea.proxy_actions.ProxyMouse
NoAction = aenea.proxy_actions.NoAction
MousePhantomClick = aenea.proxy_actions.ProxyMousePhantomClick
ContextAction = aenea.proxy_actions.ProxyContextAction

AppContext = aenea.proxy_contexts.ProxyAppContext
CustomAppContext = aenea.proxy_contexts.ProxyCustomAppContext
AlwaysContext = aenea.proxy_contexts.AlwaysContext
NeverContext = aenea.proxy_contexts.NeverContext

__all__ = [
    # Classes not present in Dragonfly.
    'NoAction',
    'MousePhantomClick',
    'ContextAction',
    'CustomAppContext',
    'AlwaysContext',
    'NeverContext',

    # Classes present in Dragonfly.
    'Grammar',
    'ConnectionGrammar',

    'Rule',
    'CompoundRule',
    'MappingRule',

    'ElementBase',
    'Sequence',
    'Alternative',
    'Optional',
    'Repetition',
    'Literal',
    'RuleRef',
    'ListRef',
    'Dictation',
    'DictListRef',
    'Compound',
    'Choice',

    'ActionBase',
    'Repeat',
    'Key',
    'Text',
    'Paste',
    'Mouse',
    'Function',
    'Mimic',
    'Playback',
    # 'WaitWindow',
    # 'FocusWindow',
    'Pause',

    'Context',
    'AppContext',
    ]

