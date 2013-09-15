from proxy_nicknames import *
from dragonfly import (Grammar, AppContext, CompoundRule, Choice, Dictation, List, Optional, Literal, Context, Repetition, MappingRule, RuleRef, DictListRef, DictList, Alternative)
import natlink, os
vim = __import__("_vim")
from comsat import ComSat

from raul import SelfChoice
import raul

# SSH_LOVED_HOSTS = {"my favorite server":"my.favorite.server.com"}
from personal import SSH_LOVED_HOSTS

# for use with autojump
# JUMP_PLACES = {"spoken form":"written form"}
from personal import JUMP_PLACES

class ShellContext(Context):
  def __init__(self):
    self._str = "ShellContext"

  def matches(self, executable, title, handle):
    with ComSat() as cs:
      return cs.getRPCProxy().callGetState()["in_terminal"]

grammar_context = (AppContext(executable="notepad") & ShellContext()) & (~vim.vim_context)
grammar = Grammar("shell", context=grammar_context)

class ChangeDirectory(CompoundRule):
  spec = "kid [<place> [<lists>] ]"
  lists = {"lists":" && ls"}
  place = {"up":"..", "back":"-", "temp":"/tmp", "home":"~"}

  extras = [SelfChoice("place", place), SelfChoice("lists", lists)]

  def _process_recognition(self, node, extras):
    place = self.place.get(str(extras.get("place", "")), "")
    lists = self.lists.get(str(extras.get("lists", "")), "")

    with ComSat() as connection:
      connection.getRPCProxy().callText("cd %s %s" % (place, lists))

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

    with ComSat() as connection:
      if flags != "slap":
        suffix += " "
      connection.getRPCProxy().callText("ls %s %s" % (flags, suffix))

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

    with ComSat() as connection:
      connection.getRPCProxy().callText("git %s %s" % (command, option_string))

      if "m" in options:
        connection.getRPCProxy().callKeys(["Left"])

class ShellJump(CompoundRule):
  spec = "jump [<place>]"
  place = JUMP_PLACES

  extras = [SelfChoice("place", place)]

  def _process_recognition(self, node, extras):
    place = self.place.get(str(extras.get("place", "")), "")

    with ComSat() as connection:
      connection.getRPCProxy().callText("j %s" % place)

class SimpleCommand(CompoundRule):
  spec = "[<sudo>] <command>"
  commands = {"remove dirt":"rmdir", "make dirt":"mkdir", "copy":"cp",
             "move":"mv", "rim":"rm", "unlink":"rm", "editor":"gvim", "editor console":"vim",
             "grep":"grep", "grubby":"grep -i", "grubber":"grep -r",
             "grubby ear":"grep -ir", "find":"find", "said it":"sed -e", "unique":"uniq",
             "awkward":"awk", "diff":"diff", "sort":"sort", "x arguments":"xargs",
             "tar":"tar", "gzip":"gzip", "be zip":"bzip2", "elziemay":"lzma",
             "zip":"zip", "zipper":"zip -r", "unzip":"unzip", "edit cron tab":"crontab -e -u alexr",
             "shall echo":"echo", "edit root cron tab":"sudo crontab -u root",
             "process snapshot":"ps", "table of processes":"top", "top":"top",
             "free space":"df -h", "disk usage":"du -hs .", "kill":"kill",
             "cat":"cat", "mount":"mount", "you mount":"umount",
             "ch mod":"chmod", "ch own":"chown", "ch mod a plus x":"chmod a+x",
             "man":"man", "less":"less", "ping":"ping", "w get":"wget",
             "apt cache search":"apt-cache search", "apt get update":"apt-get update",
             "pipe grep":" | grep -i", "find grep":"find | grep -i",
             "pipe":" | ", "reader":" > ", "apt get upgrade":"apt-get upgrade",
             "apt get dist upgrade":"apt-get dist-upgrade",
             "apt get install":"apt-get install", "pea socks":"ps aux | grep -i",
             "screen resume":"screen -r", "screen list":"screen -li",
             "screen start":"screen", "load key map":"xmodmap ~/.keymap",
             "tab slap":"\t\n"}

  extras = [SelfChoice("command", commands), SelfChoice("sudo", ["sudo"])]

  def _process_recognition(self, node, extras):
    command = self.commands[str(extras["command"])]
    sudo = "sudo " if "sudo" in extras else ""
    with ComSat() as connection:
      connection.getRPCProxy().callText("%s%s " % (sudo, command))

class SimpleCommand2(CompoundRule):
  spec = "<command>"
  commands = {"not now":"Home ^3 Return", "never":"*c"}

  extras = [SelfChoice("command", commands)]

  def _process_recognition(self, node, extras):
    command = self.commands[str(extras["command"])]
    with ComSat() as connection:
      connection.getRPCProxy().callModifiedKeys(command)

grammar.add_rule(ListDirectoryContents())
grammar.add_rule(ShellJump())
grammar.add_rule(ChangeDirectory())
grammar.add_rule(SimpleCommand())
grammar.add_rule(Git())
grammar.add_rule(SimpleCommand2())
grammar.add_rule(SSH())

grammar.load()

def unload():
  global grammar
  if grammar: grammar.unload()
  grammar = None
