# This file is part of Aenea
#
# Aenea is free software: you can redistribute it and/or modify it under
# the terms of version 3 of the GNU Lesser General Public License as
# published by the Free Software Foundation.
#
# Aenea is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with Aenea.  If not, see <http://www.gnu.org/licenses/>.

import Tkinter as tk
import tkFont
import datetime
import threading
import ttk

import aenea

# Keys that should be translated from a TK name to the name expected by
# the server.
TRANSLATE_KEYS = {
    'space': ' ',
    'Left': 'left',
    'Right': 'right',
    'Up': 'up',
    'Down': 'down',
    'Home': 'home',
    'Next': 'pgup',
    'Prior': 'pgdown',
    'End': 'end',
    'BackSpace': 'backspace',
    'Delete': 'del',
    'quoteright': 'apostrophe',
}

# Keys that may be sent as part of a text string. Any key pressed that
# is not in the ignored set or in this mapping will be sent as a
# keypress event, which is slightly less efficient.
LITERAL_KEYS = ('abcdefghijklmnopqrstuvwxyz'
                'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.!? ')

# Keys that should be completely ignored when pressed. This has the side
# effect that Dragon commands like 'press control J' will not work via
# this cilent.
IGNORED_KEYS = ('Shift_L', 'Control_L', 'Alt_L', '??')

_config = aenea.configuration.ConfigWatcher(
    'dictation_capture_state',
    {'enabled': True})


class ProxyBuffer(object):
    def __init__(self, log=lambda msg: None):
        self.log = log
        self.text_buffer = []
        self.key_buffer = []
        self.buffer_lock = threading.Lock()
        self.buffer_ready = threading.Condition(self.buffer_lock)
        self.to_send = []
        self.aenea_worker_active = False
        self.sending = False
        threading.Thread(target=self.worker_thread).start()

    def start_capture(self):
        with self.buffer_lock:
            while self.sending:
                self.buffer_ready.wait()
            aenea.ProxyKey('Control_R').execute()

    def send_key(self, key):
        with self.buffer_lock:
            assert not self.text_buffer or not self.key_buffer
            if key in LITERAL_KEYS:
                self.flush_key_buffer()
                self.text_buffer.append(key)
            else:
                self.flush_text_buffer()
                self.key_buffer.append(key)
            self.buffer_ready.notify()

    # Requires buffer_lock
    def flush_text_buffer(self):
        if self.text_buffer:
            self.to_send.append(aenea.ProxyText(''.join(self.text_buffer)))
            self.text_buffer = []

    # Requires buffer_lock
    def flush_key_buffer(self):
        if self.key_buffer:
            try:
                self.to_send.append(aenea.ProxyKey(','.join(self.key_buffer)))
            except Exception:
                self.log("Encountered a bad key: %s" % key)
            self.key_buffer = []

    def worker_thread(self):
        while 1:
            with self.buffer_lock:

                # Wait until we have something to send.
                while not (self.text_buffer or self.key_buffer or self.to_send):
                    assert not self.text_buffer or not self.key_buffer
                    self.sending = False
                    self.buffer_ready.wait()
                    self.sending = True

                assert not self.text_buffer or not self.key_buffer
                self.flush_text_buffer()
                self.flush_key_buffer()

                todo, self.to_send = self.to_send, []

            if todo:
                for action in todo:
                    action.execute()


class AeneaClient(tk.Tk):

    def __init__(self):
        tk.Tk.__init__(self)
        self.wm_title('Aenea client - Dictation capturing')
        self.geometry('400x600+400+0')
        self.wait_visibility(self)
        note = ttk.Notebook(self)
        self.tab1 = tk.Frame(note)
        self.tab2 = tk.Frame(note)
        w = tk.LabelFrame(self.tab1, text=u'Controls')
        w.pack(side=tk.TOP, fill=tk.BOTH)
        self.button1 = tk.Button(
            w,
            text=u'Start capture',
            command=self.start_capture
            )
        self.button1.pack(side=tk.LEFT)
        self.button2 = tk.Button(
            w,
            text=u'Stop capture',
            command=self.stop_capture,
            state=tk.DISABLED
            )
        self.button2.pack(side=tk.LEFT)
        self.button3 = tk.Button(
            w,
            text=u'Clear box',
            command=self.clear_text
            )
        self.button3.pack(side=tk.LEFT)
        self.display_entered_text = tk.IntVar()
        self.checkbox1 = tk.Checkbutton(
            w,
            text='Display entered text',
            variable=self.display_entered_text
            )
        self.checkbox1.pack(side=tk.LEFT)

        dFont = tkFont.Font(family='Tahoma', size=8)

        l = tk.Label(self.tab1, text=u'Capture:')
        l.pack(side=tk.TOP)

        self.tab1.text1 = tk.Text(self.tab1, width=16, height=5, font=dFont)
        yscrollbar = tk.Scrollbar(
            self.tab1.text1,
            orient=tk.VERTICAL,
            command=self.tab1.text1.yview
            )
        yscrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        self.tab1.text1['yscrollcommand'] = yscrollbar.set
        self.tab1.text1.pack(side=tk.TOP, fill=tk.BOTH, expand=tk.YES)
        self.tab1.pack(side=tk.TOP, fill=tk.X)
        self.tab1.text1.bind('<FocusIn>', lambda event: self.focus())

        l = tk.Label(self.tab1, text=u'Log:')
        l.pack(side=tk.TOP)

        self.tab1.text2 = tk.Text(self.tab1, width=16, height=5, font=dFont)
        yscrollbar = tk.Scrollbar(
            self.tab1.text2,
            orient=tk.VERTICAL,
            command=self.tab1.text2.yview
            )
        yscrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        self.tab1.text2['yscrollcommand'] = yscrollbar.set
        self.tab1.text2.pack(side=tk.TOP, fill=tk.BOTH, expand=tk.YES)
        self.tab1.pack(side=tk.TOP,  fill=tk.X)
        self.tab1.text2.bind('<FocusIn>', lambda event: self.focus())

        l = tk.Label(self.tab2, text=u'Todo...')
        l.pack(side=tk.LEFT)

        note.add(self.tab1, text='Capturing')
        note.add(self.tab2, text='Configuration')
        note.pack(side=tk.LEFT, fill=tk.BOTH, expand=tk.YES)

        self.proxy_buffer = ProxyBuffer(log=self.log)

    def log(self, message):
        timeStamp = datetime.datetime.now()
        self.tab1.text2.insert(tk.END, '%s: %s\n' % (timeStamp, message))
        self.tab1.text2.see(tk.END)  # Scroll to end.

    def start_capture(self):
        # Release VirtualBox keyboard capture.
        self.proxy_buffer.start_capture()
        self.log('Starting capture')
        self.bind('<Any KeyPress>', lambda event: self.send_key(event.keysym))
        self.button1.config(state=tk.DISABLED)
        self.button2.config(state=tk.NORMAL)

    def stop_capture(self):
        self.log('Stopping capture')
        self.bind('<Any KeyPress>', self.dummy_event)
        self.button1.config(state=tk.NORMAL)
        self.button2.config(state=tk.DISABLED)

    def dummy_event(self, event):
        pass

    def send_key(self, key):
        _config.refresh()
        if not _config.conf.get('enabled', True):
            return

        if self.display_entered_text.get():
            self.tab1.text1.insert(tk.END, (key if key != 'space' else ' '))
            self.tab1.text1.see(tk.END)  # Scroll to end.
        if key in IGNORED_KEYS:
            return
        self.proxy_buffer.send_key(TRANSLATE_KEYS.get(key, key))

    def clear_text(self):
        self.tab1.text1.delete('1.0', tk.END)

if __name__ == '__main__':
    root = AeneaClient()
    root.mainloop()
