'''Mock module to allow importing aenea on linux (mostly so you can run tests
   locally.'''

print 'Unable to import Dragonfly (safe to ignore if running tests).'

class ActionBase(object):
    def __init__(self, spec):
        self.actions = self._parse_spec(spec)

    def execute(self, data=None):
        self._execute_events(self.actions)


class DynStrActionBase(ActionBase):
    pass


class Context(object):
    pass

DictList = lambda name: dict()
List = lambda name: list()


class _WindowInfo(object):
    executable = None
    title = None
    handle = None


class Window(object):
    @staticmethod
    def get_foreground():
        return _WindowInfo
