#!/usr/bin/python2

import jsonrpclib
import sys
import threading
import time
import Xlib
import Xlib.display
import Xlib.X

import config

WINDOW_NAME = 'Aenea X11 server integration test'
CLS_NAME = 'aenea'
CLS = 'IntegrationTest'


class Proxy(object):

    def __init__(self, host, port):
        self.server = jsonrpclib.Server('http://%s:%i' % (host, port))

    def execute_batch(self, batch):
        self.server.multiple_actions(batch)


class BatchProxy(object):

    def __init__(self):
        self._commands = []

    def __getattr__(self, key):
        def call(*a, **kw):
            if not key.startswith('_'):
                self._commands.append((key, a, kw))
        return call


class Window(object):

    def __init__(self, display):
        self.display = display
        self.objects = []

        self.screen = self.display.screen()

        self.window = self.screen.root.create_window(
            0, 0, 300, 300, 2,
            self.screen.root_depth,
            Xlib.X.InputOutput,
            Xlib.X.CopyFromParent,

            background_pixel=self.screen.white_pixel,
            event_mask=(Xlib.X.ButtonPressMask |
                        Xlib.X.ButtonReleaseMask |
                        Xlib.X.KeyPressMask |
                        Xlib.X.KeyReleaseMask |
                        Xlib.X.PointerMotionMask),
            colormap=Xlib.X.CopyFromParent,
        )

        self.WM_DELETE_WINDOW = self.display.intern_atom('WM_DELETE_WINDOW')
        self.WM_PROTOCOLS = self.display.intern_atom('WM_PROTOCOLS')

        self.window.set_wm_name(WINDOW_NAME)
        self.window.set_wm_icon_name('integration_test.py')
        self.window.set_wm_class(CLS_NAME, CLS)

        self.window.map()

        self.event_mask = set()
        self.events = self._event_loop()

    def _event_loop(self):
        while 1:
            e = self.display.next_event()

            if e.type == Xlib.X.DestroyNotify:
                sys.exit(0)

            elif e.type == Xlib.X.ClientMessage:
                if e.client_type == self.WM_PROTOCOLS:
                    fmt, data = e.data
                    if fmt == 32 and data[0] == self.WM_DELETE_WINDOW:
                        sys.exit(0)

            elif e.type in self.event_mask:
                yield e

    def flush(self):
        events = []
        while self.display.pending_events():
            events.append(self.display.next_event())
        return events

    def set_event_mask(self, events_of_interest):
        self.event_mask = set(events_of_interest)

    def wait_event(self, type):
        for e in self.events:
            if e.type == type:
                return e
            else:
                assert False

    def keysym(self, key_event):
        keysym = self.display.keycode_to_keysym(key_event.detail,
                                                key_event.state)
        return self.display.lookup_string(keysym)


def test_get_context(rpc, window):
    context = rpc.get_context()
    assert context[u'title'] == WINDOW_NAME
    assert context[u'cls_name'] == CLS_NAME
    assert context[u'cls'] == CLS


def test_key_press(rpc, window):
    window.set_event_mask([Xlib.X.KeyPress, Xlib.X.KeyRelease])

    # Simple key press and release
    rpc.key_press(key='a')
    assert window.keysym(window.wait_event(Xlib.X.KeyPress)) == 'a'
    assert window.keysym(window.wait_event(Xlib.X.KeyRelease)) == 'a'

    # Interspersed press and release
    rpc.key_press(key='a', direction='down')
    assert window.keysym(window.wait_event(Xlib.X.KeyPress)) == 'a'
    rpc.key_press(key='b', direction='down')
    assert window.keysym(window.wait_event(Xlib.X.KeyPress)) == 'b'
    rpc.key_press(key='a', direction='up')
    assert window.keysym(window.wait_event(Xlib.X.KeyRelease)) == 'a'
    rpc.key_press(key='c', direction='down')
    assert window.keysym(window.wait_event(Xlib.X.KeyPress)) == 'c'
    rpc.key_press(key='c', direction='up')
    assert window.keysym(window.wait_event(Xlib.X.KeyRelease)) == 'c'
    rpc.key_press(key='b', direction='up')
    assert window.keysym(window.wait_event(Xlib.X.KeyRelease)) == 'b'

    # Fun with manual shifting
    rpc.key_press(key='shift', direction='down')
    window.wait_event(Xlib.X.KeyPress)
    rpc.key_press(key='a')
    assert window.keysym(window.wait_event(Xlib.X.KeyPress)) == 'A'
    rpc.key_press(key='shift', direction='up')
    window.wait_event(Xlib.X.KeyRelease)

    # Automatic shifting
    window.set_event_mask([Xlib.X.KeyPress])
    rpc.key_press(key='a', modifiers=['shift'])
    window.wait_event(Xlib.X.KeyPress)  # shift down
    assert window.keysym(window.wait_event(Xlib.X.KeyPress)) == 'A'
    window.set_event_mask([Xlib.X.KeyPress, Xlib.X.KeyRelease])
    assert window.keysym(window.wait_event(Xlib.X.KeyRelease)) == 'A'
    window.wait_event(Xlib.X.KeyRelease)  # shift up

    # Repeat
    rpc.key_press(key='x', count=5)
    for i in xrange(5):
        assert window.keysym(window.wait_event(Xlib.X.KeyPress)) == 'x'
        assert window.keysym(window.wait_event(Xlib.X.KeyRelease)) == 'x'


def test_write_text(rpc, window):
    window.set_event_mask([Xlib.X.KeyPress])
    text = "Hello world!"
    rpc.write_text(text=text)
    for expected in text:
        # We wish to ignore shifts.
        actual = None
        while not actual:
            actual = window.keysym(window.wait_event(Xlib.X.KeyPress))
        assert actual == expected, (actual, expected)
    window.flush()


def test_click_mouse(rpc, window):
    window.set_event_mask([Xlib.X.ButtonPress, Xlib.X.ButtonRelease])

    rpc.click_mouse(button='left', count=2)
    for i in xrange(2):
        assert window.wait_event(Xlib.X.ButtonPress).detail == 1
        assert window.wait_event(Xlib.X.ButtonRelease).detail == 1

    rpc.click_mouse(button='wheelup', count=3)
    for i in xrange(3):
        assert window.wait_event(Xlib.X.ButtonPress).detail == 4
        assert window.wait_event(Xlib.X.ButtonRelease).detail == 4

    rpc.click_mouse(button='wheeldown')
    assert window.wait_event(Xlib.X.ButtonPress).detail == 5
    assert window.wait_event(Xlib.X.ButtonRelease).detail == 5

    rpc.click_mouse(button='right')
    assert window.wait_event(Xlib.X.ButtonPress).detail == 3
    assert window.wait_event(Xlib.X.ButtonRelease).detail == 3


def test_move_mouse(rpc, window):
    # TODO: figure out a way to test the other modes of movement (e.g.
    # proportional) -- maybe xvfb?
    window.set_event_mask([Xlib.X.MotionNotify])

    # Record the initial position so we know where the window is.
    rpc.move_mouse(x=0, y=1, reference='relative')
    basis = window.wait_event(Xlib.X.MotionNotify)
    win_x = basis.root_x - basis.event_x
    win_y = basis.root_y - basis.event_y

    # Absolute move
    rpc.move_mouse(x=basis.root_x + 5, y=basis.root_y + 4)
    event = window.wait_event(Xlib.X.MotionNotify)
    assert event.root_x == basis.root_x + 5
    assert event.root_y == basis.root_y + 4
    assert event.event_x == basis.event_x + 5
    assert event.event_y == basis.event_y + 4

    # Relative move
    rpc.move_mouse(x=80, y=75, reference='relative_active')
    event = window.wait_event(Xlib.X.MotionNotify)
    assert event.event_x == 80
    assert event.event_y == 75
    assert event.root_x == win_x + 80
    assert event.root_y == win_y + 75


def test_mouse_drag(rpc, window):
    window.set_event_mask(
        [Xlib.X.MotionNotify, Xlib.X.ButtonPress, Xlib.X.ButtonRelease])

    rpc.click_mouse(button='left', direction='down')
    assert window.wait_event(Xlib.X.ButtonPress).detail == 1

    rpc.move_mouse(x=-10, y=1, reference='relative')
    drag_event = window.wait_event(Xlib.X.MotionNotify)
    assert drag_event.state != 0

    rpc.click_mouse(button='left', direction='up')
    assert window.wait_event(Xlib.X.ButtonRelease).detail == 1

    rpc.move_mouse(x=10, y=-2, reference='relative')
    drag_event = window.wait_event(Xlib.X.MotionNotify)
    assert drag_event.state == 0


def test_pause(rpc, window):
    window.set_event_mask([Xlib.X.KeyPress])

    ts = time.time()
    rpc.pause(amount=334)
    rpc.key_press(key='a')
    window.wait_event(Xlib.X.KeyPress)
    assert time.time() - ts >= 0.334
    window.flush()


def test_multiple_actions(sync_rpc, window):
    window.set_event_mask([Xlib.X.KeyPress, Xlib.X.MotionNotify])

    rpc = BatchProxy()

    rpc.key_press(key='a')
    rpc.pause(amount=134)
    rpc.key_press(key='b')
    rpc.pause(amount=192)
    rpc.move_mouse(x=-10, y=1, reference='relative')
    rpc.pause(amount=534)
    rpc.key_press(key='c')

    def perform_io(sync_rpc, cmds):
        sync_rpc.multiple_actions(cmds)

    t = threading.Thread(target=perform_io, args=(sync_rpc, rpc._commands))
    t.start()

    ts = time.time()
    assert window.keysym(window.wait_event(Xlib.X.KeyPress)) == 'a'
    assert window.keysym(window.wait_event(Xlib.X.KeyPress)) == 'b'
    assert time.time() - ts >= 0.134, time.time() - ts
    ts = time.time()
    window.wait_event(Xlib.X.MotionNotify)
    assert time.time() - ts >= 0.192
    ts = time.time()
    assert window.keysym(window.wait_event(Xlib.X.KeyPress)) == 'c'
    assert time.time() - ts >= 0.534

    t.join()

    window.flush()


def test(method, rpc, window):
    print 'Testing', method.__name__
    assert not window.flush()
    window.set_event_mask([])
    method(rpc, window)
    assert not window.flush()
    window.set_event_mask([])


if __name__ == '__main__':
    window = Window(Xlib.display.Display())
    window.flush()

    communication = Proxy(config.HOST, config.PORT)

    print 'Start the server, maximize the test window on your primary monitor, '
    print 'put the mouse in the center, select the window, and then hit space '
    print 'to begin the automated test.'
    print
    window.set_event_mask([Xlib.X.KeyPress, Xlib.X.KeyRelease])
    for event in window.events:
        if event.type == Xlib.X.KeyRelease and window.keysym(event) == ' ':
            break

    print 'Beginning test.'

    test(test_get_context, communication.server, window)
    test(test_key_press, communication.server, window)
    test(test_write_text, communication.server, window)
    test(test_click_mouse, communication.server, window)
    test(test_move_mouse, communication.server, window)
    test(test_mouse_drag, communication.server, window)
    test(test_pause, communication.server, window)
    test(test_multiple_actions, communication.server, window)

    print 'All tests complete.'
