class DynStrActionBase(object):
  def __init__(self, spec):
    self.actions = self._parse_spec(spec)

  def execute(self):
    self._execute_events(self.actions)
