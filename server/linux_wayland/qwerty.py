from abstractKeyboardMapping import AbstractKeyboardMapping
import evdev

class Qwerty(AbstractKeyboardMapping):
	def __init__(self):
		super(AbstractKeyboardMapping, self).__init__()

	def solo(self):
		return { "!" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_1],
		         "@" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_2],
		         "#" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_3],
		         "$" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_4],
		         "%" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_5],
		         "^" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_6],
		         "&" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_7],
		         "*" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_8],
		         "(" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_9],
		         ")" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_0],
		         "_" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_MINUS],
		         "+" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_EQUAL],

		         "{" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_LEFTBRACE],
		         "}" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_RIGHTBRACE],
		         ":" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_SEMICOLON],
		         "\"" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_APOSTROPHE],
		         "|" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_BACKSLASH],

		         "<" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_COMMA],
		         ">" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_DOT],
		         "?" : [evdev.ecodes.KEY_LEFTSHIFT, evdev.ecodes.KEY_SLASH],

		}

	def multi(self):
		#no multi keys I think
		return {}
