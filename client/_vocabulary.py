'''Manages Aenea's dynamic and static vocabulary and their global context.
   While this grammar is not strictly necessary to use them, you'll need it if
   you want a global context or being able to reload configurations on the
   fly rather than restarting Dragon.'''

import aenea.vocabulary

import dragonfly

vocabulary_list = aenea.vocabulary.register_list_of_dynamic_vocabularies()


class RefreshRule(dragonfly.CompoundRule):
    spec = '[refresh|reload] dynamic [vocabulary|vocabularies]'

    def _process_begin(self):
        # Refresh every time the user starts to say anything. Refresh is
        # indeed intended to be used thus. Short of inotify and threads, this
        # is how to do it.
        aenea.vocabulary.refresh_vocabulary()

    def _process_recognition(self, node, extras):
        aenea.vocabulary.refresh_vocabulary(force_reload=True)


class EnableRule(dragonfly.CompoundRule):
    spec = 'enable vocabulary <vocabulary>'
    extras = [dragonfly.ListRef('vocabulary', vocabulary_list)]

    def _process_recognition(self, node, extras):
        aenea.vocabulary.enable_dynamic_vocabulary(extras['vocabulary'])


class DisableRule(dragonfly.CompoundRule):
    spec = 'disable vocabulary <vocabulary>'
    extras = [dragonfly.ListRef('vocabulary', vocabulary_list)]

    def _process_recognition(self, node, extras):
        aenea.vocabulary.disable_dynamic_vocabulary(extras['vocabulary'])


class DynamicRule(dragonfly.Rule):
    element = dragonfly.DictListRef(
        'dynamic global',
        aenea.vocabulary.register_global_dynamic_vocabulary()
        )


class StaticRule(dragonfly.Rule):
    element = dragonfly.DictListRef(
        'static global',
        dragonfly.DictList(
            'static global',
            aenea.vocabulary.get_static_vocabulary('global')
            )
        )


grammar = dragonfly.Grammar('vocabulary')
grammar.add_rule(RefreshRule())
grammar.add_rule(EnableRule())
grammar.add_rule(DisableRule())

grammar.add_rule(DynamicRule())
grammar.add_rule(StaticRule())

grammar.load()


# Unload function which will be called at unload time.
def unload():
    aenea.vocabulary.unregister_list_of_dynamic_vocabularies()
    global grammar
    if grammar:
        grammar.unload()
    grammar = None
