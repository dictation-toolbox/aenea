from dragonfly import (Grammar, AppContext, CompoundRule, Choice, Dictation, List, Optional, Literal, Context, Repetition, MappingRule)
import dragonfly
from proxy_nicknames import *

import aenea

thunderbird_context = AppContext(window_class="Icedove") & aenea.global_context
mail_context = thunderbird_context & AppContext(window_class_name="Mail")
compose_context = thunderbird_context  & AppContext(window_class_name="Msgcompose")

email_grammar = Grammar("email", context=mail_context)
compose_grammar = Grammar("composed", context=compose_context)

class MailCommands(MappingRule):
  mapping = {
      "inbox":                 MousePhantomClick("(78 114), 1"),
      "lists":                 MousePhantomClick("(88 225), 1"),
      "limbo":                 MousePhantomClick("(88 205), 1"),
      "twenty thirteen":       MousePhantomClick("(88 365), 1"),
      "mail":                  MousePhantomClick("(500 115), 1"),
      "body":                  MousePhantomClick("(283 650), 1"),
      "compose":               Key("c-m"),
      "pane":                  Key("f6"),
      "message pane":          Key("f8"),
      "reply":                 Key("cs-r"),
      "reply to sender":       Key("c-r"),
      "spam":                  Key("j"),
      "not spam":              Key("s-j"),
      "find":                  Key("c-f"),
      "search":                Key("cs-k"),
      "expand thread":         Key("right"),
      "collapse thread":       Key("left")
      }

class ComposeCommands(MappingRule):
  mapping = {
      "sign":                  Key("cs-s"),
      "encrypt":               Key("cs-e"),
      "sign and encrypt":      Key("cs-s, cs-e"),
      "encrypt and sign":      Key("cs-s, cs-e"),
      "send message":          Key("c-enter"),
      }

email_grammar.add_rule(MailCommands())
compose_grammar.add_rule(ComposeCommands())

email_grammar.load()
compose_grammar.load()

def unload():
  global email_grammar
  global compose_grammar
  if email_grammar:
    email_grammar.unload()
  if compose_grammar:
    compose_grammar.unload()
  email_grammar = compose_grammar = None
