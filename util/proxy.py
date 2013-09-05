import comsat

class ReentrantComSatWrapper(object):
  def __init__(self):
    self._count = 0
    self._comsat = None
    self._proxy = None

  def __enter__(self):
    if self._count == 0:
      self._comsat = comsat.ComSat()
      self._proxy = self._comsat.__enter__().getRPCProxy()
    self._count += 1
    return self._proxy

  def __exit__(self, eType, eValue, eTrace):
    self._count -= 1
    if self._count == 0:
      self._comsat.__exit__(None, None, None)
      self._comsat = None
      self._proxy = None

communications = ReentrantComSatWrapper()
