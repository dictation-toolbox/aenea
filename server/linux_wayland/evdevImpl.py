import evdev
import logging
import subprocess

import time

from server.core import AbstractAeneaPlatformRpcs

from qwerty import Qwerty
from azerty import Azerty

mappings = { "qwerty" : Qwerty(),
             "azerty" : Azerty(),
}

special = { "enter" : evdev.ecodes.KEY_ENTER,
            "tab" : evdev.ecodes.KEY_TAB,
            "alt" : evdev.ecodes.KEY_LEFTALT,
            "win" : evdev.ecodes.KEY_LEFTMETA,
            "super" : evdev.ecodes.KEY_LEFTMETA,
            "shift" : evdev.ecodes.KEY_LEFTSHIFT,
            "control" : evdev.ecodes.KEY_LEFTCTRL,
            "space" : " ",
            "plus" : "+",
            "minus" : "-",
            "backspace" : evdev.ecodes.KEY_BACKSPACE,
            "del" : evdev.ecodes.KEY_DELETE,
            "lbrace" : "{",
            "rbrace" : "}",
            "left" : evdev.ecodes.KEY_LEFT,
            "right" : evdev.ecodes.KEY_RIGHT,
            "up" : evdev.ecodes.KEY_UP,
            "down" : evdev.ecodes.KEY_DOWN,
            "lparen" : "(",
            "rparen" : ")",
            "lbracket" : "[",
            "rbracket" : "]",
            "colon" : ":",
            "comma" : ",",
            "semicolon" : ";",
            "dot" : ".",
            "slash" : "/",
            "hash" : "#",
            "percent" : "%",
            "asterisk" : "*",
            "dollar" : "$",
            "backslash" : "\\",
            "apostrophe" : "'",
            "dquote" : "\"",
            "rangle" : ">",
            "langle" : "<",
            "equal" : "=",
            "exclamation" : "!",
            "question" : "?",
            "bar" : "|",
            "underscore" : "_",
            "ampersand" : "&",
            "at" : "@",
            "f1" : evdev.ecodes.KEY_F1,
            "f2" : evdev.ecodes.KEY_F2,
            "f3" : evdev.ecodes.KEY_F3,
            "f4" : evdev.ecodes.KEY_F4,
            "f5" : evdev.ecodes.KEY_F5,
            "f6" : evdev.ecodes.KEY_F6,
            "f7" : evdev.ecodes.KEY_F7,
            "f8" : evdev.ecodes.KEY_F8,
            "f9" : evdev.ecodes.KEY_F9,
            "f10" : evdev.ecodes.KEY_F10,
            "f11" : evdev.ecodes.KEY_F11,
            "f12" : evdev.ecodes.KEY_F12,
}

fixed = { "\n" : [evdev.ecodes.KEY_ENTER],
          " " : [evdev.ecodes.KEY_SPACE],
          "\t" : [evdev.ecodes.KEY_TAB],
}

_SERVER_INFO = {
    "window_manager": "sway",
    "operating_system": "linux",
    "platform": "linux",
    "display": "Wayland",
    "server": "aenea_reference",
    "server_version": 1
}

buttons = { "right" : evdev.ecodes.BTN_RIGHT,
            "left" : evdev.ecodes.BTN_LEFT,
            "middle" : evdev.ecodes.BTN_MIDDLE,
}

class EvdevPlatformRpcs(AbstractAeneaPlatformRpcs):
	def __init__(self, config, mapping, keyEvent, mouseEvent, security_token=None):
		super(EvdevPlatformRpcs, self).__init__(logger=logging.getLogger("aenea.XdotoolPlatformRpcs"), security_token=security_token)
		self.mapping = mappings.get(mapping, "qwerty")

		key = evdev.InputDevice(keyEvent)
		mouse = evdev.InputDevice(mouseEvent)

		self.ui = evdev.UInput.from_device(key, mouse)

	def server_info(self, security_token):
		return _SERVER_INFO

	def get_context(self, security_token):
		self.logger.info("get_context Not implemented yet")
		return {}


	def key_press(self,
	              key=None,
	              modifiers=(),
	              direction="press",
	              count=1,
	              count_delay=None,
	              security_token=None):
		"""press a key possibly modified by modifiers. direction may be
		'press', 'down', or 'up'. modifiers may contain 'alt', 'shift',
		'control', 'super'. this X11 server also supports 'hyper',
		'meta', and 'flag' (same as super). count is number of times to
		press it. count_delay delay in ms between presses."""
		assert key is not None

		delay_millis = 0 if count_delay is None or count == 1 else count_delay
		modifiers = [special.get(mod) for mod in modifiers]

		key = special.get(key, key) #convert to usable str or to a key code

		if type(key) is str: #need to convert to key codes
			keys = fixed.get(key)
			if keys is None: #not a fixed
				keys = self.mapping.solo().get(key)
			if keys is None: #basic key
				keys = [evdev.ecodes.ecodes["KEY_" + key.upper()]]
		else:
			keys = [key]

		for _ in range(0, count):
			#modifiers down:
			for m in modifiers:
				self.ui.write(evdev.ecodes.EV_KEY, m, 1)

			#key:
			if direction == "press" or direction == "down":
				for k in keys:
					self.ui.write(evdev.ecodes.EV_KEY, k, 1)

			if direction == "press" or direction == "up":
				for k in keys:
					self.ui.write(evdev.ecodes.EV_KEY, k, 0)

			#modifiers up:
			for m in modifiers:
				self.ui.write(evdev.ecodes.EV_KEY, m, 0)

			self.ui.syn()
			time.sleep(delay_millis / 1000.0)

	def write_text(self, text, security_token=None):
		for letter in text:
			#check if letter need more than 1 key
			seq = self.mapping.multi().get(letter)

			if seq is not None:
				for k in seq:
					self.ui.write(evdev.ecodes.EV_KEY, k[0], k[1])
			else:
				#"standard" letter
				seq = fixed.get(letter)
				if seq is None:
					seq = self.mapping.solo().get(letter)
				if seq is not None:
					#fixed key or solo.
					for k in seq:
						#keys down:
						self.ui.write(evdev.ecodes.EV_KEY, k, 1)
					for k in reversed(seq):
						#keys up:
						self.ui.write(evdev.ecodes.EV_KEY, k, 0)
				else:
					# standard key:
					if letter.isupper():
						#Press shift to have upper letter
						self.ui.write(evdev.ecodes.EV_KEY,
						         evdev.ecodes.KEY_LEFTSHIFT,
						         1)

					k = evdev.ecodes.ecodes["KEY_" + letter.upper()]
					#press key
					self.ui.write(evdev.ecodes.EV_KEY,k, 1)
					#release key
					self.ui.write(evdev.ecodes.EV_KEY,k, 0)

					if letter.isupper():
						# shift up
						self.ui.write(evdev.ecodes.EV_KEY,
						              evdev.ecodes.KEY_LEFTSHIFT,
						              0)
			self.ui.syn()
			#if no pause, some events are lost, I don't know why
			time.sleep(0.000001)


	def click_mouse(self, button, direction="click", count=1, count_delay=None, security_token=None):
		delay_millis = 0 if count_delay is None or count == 1 else count_delay
		print("click mouse " + button + " " + direction)
		for _ in range(0, count):
			b = buttons.get(button)
			if button == "wheeldown":
				self.ui.write(evdev.ecodes.EV_REL,
				              evdev.ecodes.REL_WHEEL,
				              -1)
				self.ui.syn()
			elif button == "wheelup":
				print("wheelup")
				self.ui.write(evdev.ecodes.EV_REL,
				              evdev.ecodes.REL_WHEEL,
				              1)
				self.ui.syn()
			else:
				if direction == "click" or direction == "down":
					self.ui.write(evdev.ecodes.EV_KEY,
					              b,
					              1)
					self.ui.syn()
				if direction == "click" or direction == "up":
					self.ui.write(evdev.ecodes.EV_KEY,
					              b,
					              0)
					self.ui.syn()
			time.sleep(delay_millis / 1000.0)

	def move_mouse(self,
	               x, y,
	               reference="absolute",
	               proportional=False,
	               phantom=None,
	               security_token=None):
		self.logger.info("move_mouse Not implemented yet")

	def pause(self, amount, security_token=None):
		time.sleep(amount / 1000.)

	def notify(self, message, security_token=None):
		try:
			subprocess.Popen(["notify-send", message])
		except Exception as e:
			self.logger.warn("failed to start notify-send process: %s" % e)

