class MockConfigWatcher(object):
    '''Provides similar API to ConfigWatcher but can be easilycontrolled
       by tests by changing the conf and dirty attributes.'''
    def __init__(self, path, default={}):
        self.dirty = True
        self.conf = default

    def __getitem__(self, item):
        return self.conf[item]

    def __setitem__(self, item, value):
        self.conf[item] = value

    def write(self):
        pass

    def read(self):
        self.dirty = False

    def refresh(self):
        dirty, self.dirty = self.dirty, False
        return dirty


class MockConfigDirWatcher(object):
    '''Provides similar API to ConfigDirWatcher but can be easilycontrolled
       by tests by changing the files and dirty attributes.'''

    def __init__(self, path, default={}):
        self.dirty = True
        self.files = {}

    def read(self):
        self.dirty = False

    def refresh(self):
        dirty, self.dirty = self.dirty, False
        return dirty


def make_mock_conf(conf):
    c = MockConfigWatcher(None)
    c.conf = conf
    return c


def make_mock_dir(files):
    c = MockConfigDirWatcher(None)
    c.files = dict((name, make_mock_conf(conf))
                   for (name, conf) in files.iteritems())
    return c
