
class AbstractKeyboardMapping(object):
	def __init__(self):
		return

	def solo(self):
		"""
		A solo key is a key which is different
		than qwerty keyboard or which needs a modifier
		(i.e. you don't need to release a key during the sequence)
		"""
		raise NotImplementedError()

	def multi(self):
		"""
		A multi key is a key which needs a key sequence (i.e. you need
		to press and release several keys)
		"""
		raise NotImplementedError()
