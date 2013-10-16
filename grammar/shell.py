from dragonfly import (Grammar, CompoundRule, Choice, Literal, Repetition, Text, Key, MappingRule, Alternative)

from raul import SelfChoice

# SSH_LOVED_HOSTS = {"my favorite server":"my.favorite.server.com"}
from personal import SSH_LOVED_HOSTS

# for use with autojump
# JUMP_PLACES = {"spoken form":"written form"}
from personal import JUMP_PLACES

import config

if config.PLATFORM == "proxy":
  from proxy_nicknames import *
  import aenea
  # Add your terminal here. Use xprop to determine its window class.
  # you can also add exclusions reprograms you don't want (eg, vim, ipython, etc)
  grammar_context = (AppRegexContext(window_class="(Xfce4-terminal)|(XTerm)|(URxvt)") &
                     AppRegexContext(window_class_name="(xfce4-terminal)|(xterm)|(urxvt)") &
                     ~AppRegexContext(name=".*VIM.*") &
                     ~AppRegexContext(name=".*/usr/bin/ipython.*") &
                     aenea.global_context)
  grammar = Grammar("shell", context=grammar_context)
else:
  # Add windows-appropriate context here if desired..
  grammar = Grammar("shell")

class ChangeDirectory(CompoundRule):
  spec = "kid [<place> [<lists>] ]"
  lists = {"lists":" && ls"}
  place = {"up":"..", "back":"-", "temp":"/tmp", "home":"~"}

  extras = [SelfChoice("place", place), SelfChoice("lists", lists)]

  def _process_recognition(self, node, extras):
    place = self.place.get(str(extras.get("place", "")), "")
    lists = self.lists.get(str(extras.get("lists", "")), "")

    Text("cd %s %s" % (place, lists)).execute()

class ListDirectoryContents(CompoundRule):
  spec = "lists [<flags> [<limit>] ]"
  flags = {"LH":"-lh", "L":"-l", "LHT":"-lht", "LHS":"-lhS", "slap":"\n"}
  limit = ["head", "tail", "less", "redir"]
  extras = [SelfChoice("flags", flags),
            SelfChoice("limit", limit)]

  def _process_recognition(self, node, extras):
    flags = self.flags.get(str(extras.get("flags", "")), "")
    limit = str(extras.get("limit", ""))
    
    if limit == "redir":
      suffix = " > "
    elif limit == "":
      suffix = ""
    else:
      suffix = " | " + limit

    if flags != "slap":
      suffix += " "
    Text("ls %s %s" % (flags, suffix)).execute()

class SSH(CompoundRule):
  spec = "[secure] shell [<host>]"
  extras = [Alternative(map(Literal, SSH_LOVED_HOSTS), name="host")]

  def _process_recognition(self, node, extras):
    text = "ssh "
    if "host" in extras:
      text += SSH_LOVED_HOSTS[extras["host"]]
    Text(text).execute()

class Git(CompoundRule):
  spec = "git [<command>] [<options>]"
  self_commands = ["add", "commit", "checkout", "branch", "diff", "log", "merge",
                   "pull", "push", "remote", "reset", "status"]
  # TODO: really needs to be rewritten in the syntax aware way
  for stash_command in ("list", "show", "drop", "pop", "apply", "branch", "save", "clear", ""):
    self_commands.append("stash %s" % stash_command)
  commands = {"remove":"rm", "unlink":"rm", "move":"mv", "add update":"add -u"}
  commands.update(zip(self_commands, self_commands))
  options = {"v":"v", "verbose":"v", ".":".", "here":".", "m":"m", "message":"m"}

  insect = Repetition(Choice("option", options), max=len(set(options.values())), name = "options")
  extras = [Choice("command", commands), insect]

  def _process_recognition(self, node, extras):
    command = str(extras.get("command", " "))
    options = set(extras.get("options", []))

    option_string = ""
    if "v" in options:
      option_string += " -v "
    if "." in options:
      option_string += " . "
    if "m" in options:
      option_string += " -m \"\""

    action = Text("git %s %s" % (command, option_string)).execute()
    if "m" in options:
      action += Key("left")
    action.execute()

class ShellJump(CompoundRule):
  spec = "jump [<place>]"
  place = JUMP_PLACES

  extras = [SelfChoice("place", place)]

  def _process_recognition(self, node, extras):
    place = self.place.get(str(extras.get("place", "")), "")
    Text("j " + place).execute()

class SimpleCommand(MappingRule):
  mapping = {
      "remove dirt":"rmdir",
      "make dirt":"mkdir",
      "copy":"cp",
      "move":"mv",
      "rim":"rm",
      "unlink":"rm",
      "editor":"gvim",
      "editor console":"vim",
      "grep":"grep",
      "grubby":"grep -i",
      "grubber":"grep -r",
      "grubby ear":"grep -ir",
      "find":"find",
      "said it":"sed -e",
      "unique":"uniq",
      "awkward":"awk",
      "diff":"diff",
      "sort":"sort",
      "x arguments":"xargs",
      "tar":"tar",
      "gzip":"gzip",
      "be zip":"bzip2",
      "elziemay":"lzma",
      "ecks zee":"xz",
      "zip":"zip",
      "zipper":"zip -r",
      "unzip":"unzip",
      "edit cron tab":"crontab -e -u alexr",
      "shell echo":"echo",
      "edit root cron tab":"sudo crontab -u root",
      "process snapshot":"ps",
      "pee ess":"ps",
      "table of processes":"top",
      "top":"top",
      "free space":"df -h",
      "dee eff":"df -h",
      "disk usage":"du -hs .",
      "dee you":"du -hs .",
      "kill":"kill",
      "cat":"cat",
      "mount":"mount",
      "you mount":"umount",
      "ch mod":"chmod",
      "ch own":"chown",
      "ch mod a plus x":"chmod a+x",
      "man":"man",
      "less":"less",
      "tab slap":"\t\n",
      "apt get update":"sudo apt-get update",
      "pea socks":"ps aux | grep -i",
      "apt cache search":"apt-cache search",
      "w get":"wget",
      "screen list":"screen -li",
      "screen start":"screen",
      "ping":"ping",
      "apt get dist upgrade":"sudo apt-get dist-upgrade",
      "pipe":" | ",
      "apt get upgrade":"sudo apt-get upgrade",
      "pipe grep":" | grep -i",
      "find grep":"find | grep -i",
      "reader":" > ",
      "screen resume":"screen -r",
      "load key map":"xmodmap ~/.keymap",
      "apt get install":"sudo apt-get install",
    }
  for (k, v) in mapping.items():
    mapping[k] = Text(v)
    mapping["sudo " + k] = Text("sudo " + v)

grammar.add_rule(ListDirectoryContents())
grammar.add_rule(ShellJump())
grammar.add_rule(ChangeDirectory())
grammar.add_rule(SimpleCommand())
grammar.add_rule(Git())
grammar.add_rule(SSH())

grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
