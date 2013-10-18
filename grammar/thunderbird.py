from dragonfly import Grammar, AppContext, MappingRule, Mouse, Key

import config

if config.PLATFORM == "proxy":
  from proxy_nicknames import *
  import aenea
  thunderbird_context = ((AppContext(title="Icedove") |
                          AppContext(title="Thunderbird")) & aenea.global_context)
  mail_context = thunderbird_context & AppContext(cls_name="Mail")
  compose_context = thunderbird_context  & AppContext(cls_name="Msgcompose")
else:
  thunderbird_context = AppContext(name="Thunderbird")
  mail_context = thunderbird_context & AppContext(name="Mail")
  compose_context = thunderbird_context  & AppContext(name="Msgcompose")
  MousePhantomClick = Mouse

email_grammar = Grammar("email", context=mail_context)
compose_grammar = Grammar("composed", context=compose_context)

class MailCommands(MappingRule):
  mapping = {
      "inbox":                 MousePhantomClick("(78 114), left"),
      "lists":                 MousePhantomClick("(88 225), left"),
      "limbo":                 MousePhantomClick("(88 205), left"),
      "twenty thirteen":       MousePhantomClick("(88 365), left"),
      "mail":                  MousePhantomClick("(500 115), left"),
      "body":                  MousePhantomClick("(283 650), left"),
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
