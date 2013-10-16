from dragonfly import (Grammar, AppContext, CompoundRule, Choice, Dictation, List, Optional, Literal)
import natlink, os

def SelfChoice(name, ch):
  return Choice(name, dict(zip(ch, ch)))

def processDictation(message):
  """For some reason the format() method on DictationContainers doesn't seem to
     do this right. More duct tape! More I say!"""
  subs = {".\\period":".",
          ",\\comma":",",
          "?\\question-mark":"?",
          "std::\\":"std::",
          ".\\dot":".",
          "gzip\\":"gzip",
          "bzip\\":"bzip",
          "!\\exclamation-mark":"!",
          "&\\ampersand":"&",
          "write\\":"write",
          "*\\asterisk":"*",
          "*\\asterisk":"*"}
  message = str(message).lower()
  for sub in subs.iteritems():
    message = message.replace(*sub)

  while "\\uppercase-letter" in message:
    i = message.find("\\uppercase-letter")
    if i > 0:
      message = message[:i - 1] + message[i - 1].upper() + message[i + 17:]
    else:
      message = message[17:]

  # need to get rid of Dictation for numbers first
#  return " ".join(word.split("\\")[0] for word in message)
  return message

LETTERS = ["alpha", "bravo", "charlie", "delta", "echo", "foxtrot", "golf", "hotel",
           "indigo", "juliet", "kilo", "lima", "mike", "november", "oscar",
           "poppa", "quiche", "romeo", "sierra", "tango", "uniform",
           "vector", "whiskey", "x-ray", "yankee", "zulu"]
LETTERS = dict(zip(LETTERS, (chr(ord("a") + i) for i in range(26))))
temp = {}
for (spoken, written) in LETTERS.iteritems():
  temp[spoken] = written
  temp["upper " + spoken] = written.upper()
LETTERS = temp

DIGITS = ["zero", "one", "to", "3", "for", "5", "6", "7", "8", "nine"]
DIGITS = dict(zip(DIGITS, (chr(ord("0") + i) for i in range(10))))
DIGITS["niner"] = "9"

ALPHANUMERIC = LETTERS.copy()
ALPHANUMERIC.update(DIGITS)

ALPHANUMERIC_EXTENDED = ALPHANUMERIC.copy()

ALPHANUMERIC_EXTENDED["enter"] = "Return"
ALPHANUMERIC_EXTENDED["comma"] = ","

# hackity hack hack
# TODO: replace this nonsense with dragonfly integers 
NUMBERS = {'thirty one': '31', "zero":"0", "for":"4", "when":"1", "to":"2", "two":"2", "too":"2", 'seventy nine': '79', 'fifty': '50', 'four': '4', 'ninety six': '96', 'eighty one': '81', 'fifty six': '56', 'sixty two': '62', 'twenty two': '22', 'forty eight': '48', 'twenty seven': '27', 'seventy five': '75', 'seventy three': '73', 'five': '5', 'twenty five': '25', 'sixteen': '16', 'eighty six': '86', 'twenty nine': '29', 'sixty four': '64', 'eighty four': '84', 'forty one': '41', 'twelve': '12', 'seventeen': '17', 'fifty three': '53', 'ten': '10', 'ninety one': '91', 'sixty three': '63', 'thirty seven': '37', 'ninety nine': '99', 'thirteen': '13', 'thirty six': '36', 'thirty three': '33', 'forty': '40', 'forty five': '45', 'ninety eight': '98', 'seventy six': '76', 'eighty three': '83', 'fourteen': '14', 'forty seven': '47', 'sixty': '60', 'fifteen': '15', 'seventy': '70', 'fifty one': '51', 'sixty seven': '67', 'thirty': '30', 'eighty eight': '88', 'ninety five': '95', 'thirty nine': '39', 'one': '1', 'ninety': '90', 'forty two': '42', 'fifty nine': '59', 'ninety seven': '97', 'twenty': '20', 'twenty six': '26', 'seventy seven': '77', 'eleven': '11', 'ninety four': '94', 'fifty four': '54', 'seventy one': '71', 'seventy four': '74', 'nineteen': '19', 'eighty nine': '89', 'fifty seven': '57', 'forty three': '43', 'fifty five': '55', 'twenty one': '21', 'sixty six': '66', 'fifty two': '52', 'nine': '9', 'three': '3', 'forty four': '44', 'sixty one': '61', 'ninety two': '92', 'seven': '7', 'seventy two': '72', 'sixty eight': '68', 'forty nine': '49', 'eighteen': '18', 'thirty four': '34', 'eighty seven': '87', 'ninety three': '93', 'six': '6', 'eighty two': '82', 'fifty eight': '58', 'twenty four': '24', 'eighty': '80', 'sixty nine': '69', 'eight': '8', 'two': '2', 'forty six': '46', 'thirty two': '32', 'twenty eight': '28', 'thirty five': '35', 'thirty eight': '38', 'eighty five': '85', 'seventy eight': '78', 'sixty five': '65', 'twenty three': '23'}

for k in NUMBERS.keys():
  bees = k.split()
  if len(bees) == 2:
    a = [bees[0]]
    b = [bees[1]]
    if bees[0] in NUMBERS:
      a.append(NUMBERS[bees[0]])
    if bees[1] in NUMBERS:
      b.append(NUMBERS[bees[1]])
    for i in a:
      for j in b:
        NUMBERS["%s %s" % (i, j)] = NUMBERS[k]

for i in range(100):
  NUMBERS[str(i)] = str(i)




