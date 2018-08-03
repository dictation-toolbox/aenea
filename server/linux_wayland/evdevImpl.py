import evdev
import logging
import os
import subprocess

import time

import config

import sys
from server.core import AbstractAeneaPlatformRpcs

_SERVER_INFO = {
    'window_manager': 'sway',
    'operating_system': 'linux',
    'platform': 'linux',
    'display': 'Wayland',
    'server': 'aenea_reference',
    'server_version': 1
}

upper = { '1': evdev.ecodes.ecodes['KEY_1'],
          '2': evdev.ecodes.ecodes['KEY_2'],
          '3': evdev.ecodes.ecodes['KEY_3'],
          '4': evdev.ecodes.ecodes['KEY_4'],
          '5': evdev.ecodes.ecodes['KEY_5'],
          '6': evdev.ecodes.ecodes['KEY_6'],
          '7': evdev.ecodes.ecodes['KEY_7'],
          '8': evdev.ecodes.ecodes['KEY_8'],
          '9': evdev.ecodes.ecodes['KEY_9'],
          '0': evdev.ecodes.ecodes['KEY_0'],
          '°': evdev.ecodes.ecodes['KEY_MINUS'],
          '+': evdev.ecodes.ecodes['KEY_EQUAL'],
          '£': evdev.ecodes.ecodes['KEY_RIGHTBRACE'],
          'M': evdev.ecodes.ecodes['KEY_SEMICOLON'],
          '%': evdev.ecodes.ecodes['KEY_APOSTROPHE'],
          'µ': evdev.ecodes.ecodes['KEY_BACKSLASH'],
          '?': evdev.ecodes.ecodes['KEY_M'],
          '.': evdev.ecodes.ecodes['KEY_COMMA'],
          '/': evdev.ecodes.ecodes['KEY_DOT'],
          '§': evdev.ecodes.ecodes['KEY_SLASH'],
}

lower = { '&': evdev.ecodes.ecodes['KEY_1'],
          'é': evdev.ecodes.ecodes['KEY_2'],
          '"': evdev.ecodes.ecodes['KEY_3'],
          '\'': evdev.ecodes.ecodes['KEY_4'],
          '(': evdev.ecodes.ecodes['KEY_5'],
          '-': evdev.ecodes.ecodes['KEY_6'],
          'è': evdev.ecodes.ecodes['KEY_7'],
          '_': evdev.ecodes.ecodes['KEY_8'],
          'ç': evdev.ecodes.ecodes['KEY_9'],
          'à': evdev.ecodes.ecodes['KEY_0'],
          ')': evdev.ecodes.ecodes['KEY_MINUS'],
          '=': evdev.ecodes.ecodes['KEY_EQUAL'],
          '$': evdev.ecodes.ecodes['KEY_RIGHTBRACE'],
          'm': evdev.ecodes.ecodes['KEY_SEMICOLON'],
          'ù': evdev.ecodes.ecodes['KEY_APOSTROPHE'],
          '*': evdev.ecodes.ecodes['KEY_BACKSLASH'],
          ',': evdev.ecodes.ecodes['KEY_M'],
          ';': evdev.ecodes.ecodes['KEY_COMMA'],
          ':': evdev.ecodes.ecodes['KEY_DOT'],
          '!': evdev.ecodes.ecodes['KEY_SLASH'],
}

altgr = { '¹': evdev.ecodes.ecodes['KEY_1'],
          '~': evdev.ecodes.ecodes['KEY_2'],
          '#"': evdev.ecodes.ecodes['KEY_3'],
          '{': evdev.ecodes.ecodes['KEY_4'],
          '[': evdev.ecodes.ecodes['KEY_5'],
          '|': evdev.ecodes.ecodes['KEY_6'],
          '`': evdev.ecodes.ecodes['KEY_7'],
          '\\': evdev.ecodes.ecodes['KEY_8'],
          '^': evdev.ecodes.ecodes['KEY_9'],
          '@': evdev.ecodes.ecodes['KEY_0'],
          ']': evdev.ecodes.ecodes['KEY_MINUS'],
          '}': evdev.ecodes.ecodes['KEY_EQUAL'],
}


class EvdevPlatformRpcs(AbstractAeneaPlatformRpcs):
	def __init__(self, config):
		super(EvdevPlatformRpcs, self).__init__(logger=logging.getLogger('aenea.XdotoolPlatformRpcs'))
		self.ui = evdev.UInput()


	def keyMod(self, letters, downNotUp): #down = 1, up = 0
		escape = False
		special = False
		for letter in letters:
			if letter == ' ':
				key = evdev.ecodes.KEY_SPACE
			elif letter == '\\':
				if escape == False:
					escape = True
					continue
				else:
					key = evdev.ecodes.KEY_8
			elif letter == '@':
				if special == False:
					special = True
					continue
				else:
					key = evdev.ecodes.KEY_0
			elif escape:
				switcher = {
					'r': evdev.ecodes.KEY_ENTER,
					't': evdev.ecodes.KEY_TAB,
					'n': evdev.ecodes.KEY_ENTER,
				}
				escape = False
				key = switcher.get(letter)
			elif special:
				switcher = {
					'a': evdev.ecodes.KEY_LEFTALT,
					'b': evdev.ecodes.KEY_BACKSPACE,
					'c': evdev.ecodes.KEY_LEFTCTRL,
					'd': evdev.ecodes.KEY_DELETE,
					'l': evdev.ecodes.KEY_LEFT,
					'o': evdev.ecodes.KEY_DOWN,
					'r': evdev.ecodes.KEY_RIGHT,
					's': evdev.ecodes.KEY_LEFTSHIFT,
					'u': evdev.ecodes.KEY_UP,
					'w': evdev.ecodes.KEY_LEFTMETA,
				}
				special = False
				key = switcher.get(letter)
			elif letter in upper:
				key = upper[letter]
			elif letter in lower:
				key = lower[letter]
			elif letter in altgr:
				key = altgr[letter]
			else:
				key = evdev.ecodes.ecodes['KEY_'+letter.upper()]

			if letter.isupper() or letter in upper:
				self.ui.write(evdev.ecodes.EV_KEY,
				              evdev.ecodes.KEY_LEFTSHIFT,
				              downNotUp)
			if letter in altgr:
				self.ui.write(evdev.ecodes.EV_KEY,
				              evdev.ecodes.KEY_RIGHTALT,
				              downNotUp)
			self.ui.write(evdev.ecodes.EV_KEY, key, downNotUp)


	def server_info(self):
		return _SERVER_INFO

	def get_context(self):
		self.logger.info('get_context Not implemented yet')
		return {}

	def convert_key(self, key):
		keys = {
			'enter' : '\\n',
			'tab' : '\\t',
			'alt' : '@a',
			'win' : '@w',
			'super' : '@w',
			'shift' : '@s',
			'control' : '@c',
			'@' : '@@',
			'\\' : '\\\\',
			'space' : ' ',
			'plus' : '+',
			'minus' : '-',
			'backspace' : '@b',
			'del' : '@d',
			'lbrace' : '{',
			'rbrace' : '}',
			'left' : '@l',
			'right' : '@r',
			'up' : '@u',
			'down' : '@o',
			'lparen' : '(',
			'rparen' : ')',
			'lbracket' : '[',
			'rbracket' : ']',
			'colon' : ":",
			'comma' : ',',
			'semicolon' : ';',
			'dot' : '.',
			'slash' : '/',
			'hash' : '#',
			'percent' : '%',
			'asterisk' : '*',
			'dollar' : '$',
			'backslash' : '\\\\',
			'apostrophe' : '\'',
			'dquote' : '"',
			'rangle' : '>',
			'langle' : '<',
			'equal' : '=',
			'exclamation' : '!',
			'question' : '?',
			'bar' : '|',
			'underscore' : '_',
			'ampersand' : '&',
			'at' : '@@',
			#clavier français:
			'a' : 'q',
			'A' : 'Q',
			'q' : 'a',
			'Q' : 'A',
			'z' : 'w',
			'w' : 'z',
			'Z' : 'W',
			'W' : 'Z',
		}
		if key in keys:
			return keys[key]
		return key

	def key_press(self,
	              key=None,
	              modifiers=(),
	              direction='press',
	              count=1,
	              count_delay=None):
		'''press a key possibly modified by modifiers. direction may be
		'press', 'down', or 'up'. modifiers may contain 'alt', 'shift',
		'control', 'super'. this X11 server also supports 'hyper',
		'meta', and 'flag' (same as super). count is number of times to
		press it. count_delay delay in ms between presses.'''
		assert key is not None

		key = self.convert_key(key)

		if direction == 'press' or direction == 'down':
			for m in modifiers:
				mod = self.convert_key(m)
				self.keyMod(mod, 1)
			self.keyMod(key, 1)

		if direction == 'press' or direction == 'up':
			self.keyMod(key, 0)
			for m in modifiers:
				mod = self.convert_key(m)
				self.keyMod(mod, 0)

		self.ui.syn()

	def write_text(self, text):
		for letter in text:
			key = self.convert_key(letter)
			self.keyMod(key, 1)
			self.keyMod(key, 0)
			self.ui.syn()


	def click_mouse(self, button, direction='click', count=1, count_delay=None):
		self.logger.info('click_mouse Not implemented yet')

	def move_mouse(self,
	               x, y,
	               reference='absolute',
	               proportional=False,
	               phantom=None):
		self.logger.info('move_mouse Not implemented yet')

	def pause(self, amount):
		time.sleep(amount / 1000.)

	def notify(self, message):
		try:
			subprocess.Popen(['notify-send', message])
		except Exception as e:
			self.logger.warn('failed to start notify-send process: %s' % e)

