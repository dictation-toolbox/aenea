import Tkinter as tk
import tkFont
import datetime
import sys
import threading
import ttk

import communications
import config

# Keys that should be translated from a TK name to the name expected
# by the server.
TRANSLATE_KEYS = {
    "space": " ",
    "Left": "left",
    "Right": "right",
    "Up": "up",
    "Down": "down",
    "Home": "home",
    "Next": "pgup",
    "Prior": "pgdown",
    "End": "end",
    "BackSpace": "backspace",
    "Delete": "delete",
}

# Keys that may be sent as part of a text string. Any key pressed that is not in
# the ignored set or in this mapping will be sent as a keypress event, which is
# slightly less efficient.
LITERAL_KEYS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.!? "

# Keys that should be completely ignored when pressed. This has the side effect
# that Dragon commands like "press control J" will not work via this cilent.
IGNORED_KEYS = ('Shift_L', 'Control_L', 'Alt_L', '??')

class AeneaClient(tk.Tk):

    def __init__(self, ip, port):
        tk.Tk.__init__(self)
        self.aenea_buffer = []
        self.buffer_lock = threading.Lock()
        self.buffer_ready = threading.Condition(self.buffer_lock)
        self.aenea_worker_active = False
        self.wm_title("Aenea client - Dictation capturing")
        self.geometry('400x600+400+0')
        self.wait_visibility(self)
        note = ttk.Notebook(self)
        self.tab1 = tk.Frame(note)
        self.tab2 = tk.Frame(note)
        w = tk.LabelFrame(self.tab1, text=u"Controls")
        w.pack(side=tk.TOP, fill=tk.BOTH)
        self.button1 = tk.Button(
                w,
                text=u"Start capture",
                command=self.start_capture
            )
        self.button1.pack(side=tk.LEFT)
        self.button2 = tk.Button(
                w,
                text=u"Stop capture",
                command=self.stop_capture,
                state=tk.DISABLED
            )
        self.button2.pack(side=tk.LEFT)
        self.button3 = tk.Button(
                w,
                text=u"Clear box",
                command=self.clear_text
            )
        self.button3.pack(side=tk.LEFT)
        self.display_entered_text = tk.IntVar()
        self.checkbox1 = tk.Checkbutton(
                w,
                text="Display entered text",
                variable=self.display_entered_text
            )
        self.checkbox1.pack(side=tk.LEFT)

        dFont = tkFont.Font(family="Tahoma", size=8)

        l = tk.Label(self.tab1, text=u"Capture:")
        l.pack(side=tk.TOP)

        self.tab1.text1 = tk.Text(self.tab1, width=16, height=5, font=dFont)
        yscrollbar = tk.Scrollbar(
                self.tab1.text1,
                orient=tk.VERTICAL,
                command=self.tab1.text1.yview
            )
        yscrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        self.tab1.text1["yscrollcommand"] = yscrollbar.set
        self.tab1.text1.pack(side=tk.TOP, fill=tk.BOTH, expand=tk.YES)
        self.tab1.pack(side=tk.TOP, fill=tk.X)
        self.tab1.text1.bind("<FocusIn>", lambda event: self.focus())

        l = tk.Label(self.tab1, text=u"Log:")
        l.pack(side=tk.TOP)

        self.tab1.text2 = tk.Text(self.tab1, width=16, height=5, font=dFont)
        yscrollbar = tk.Scrollbar(
                self.tab1.text2,
                orient=tk.VERTICAL,
                command=self.tab1.text2.yview
            )
        yscrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        self.tab1.text2["yscrollcommand"] = yscrollbar.set
        self.tab1.text2.pack(side=tk.TOP, fill=tk.BOTH, expand=tk.YES)
        self.tab1.pack(side=tk.TOP,  fill=tk.X)
        self.tab1.text2.bind("<FocusIn>", lambda event: self.focus())

        l = tk.Label(self.tab2, text=u"Todo...")
        l.pack(side=tk.LEFT)

        note.add(self.tab1, text="Capturing")
        note.add(self.tab2, text="Configuration")
        note.pack(side=tk.LEFT, fill=tk.BOTH, expand=tk.YES)

        try:
            self.client = communications.Proxy(ip, int(port))
            self.client_proxy = communications.BatchProxy()
        except Exception as e:
            self.log(str(e))

        threading.Thread(target=self.worker_thread).start()

    def log(self, message):
        timeStamp = datetime.datetime.now()
        self.tab1.text2.insert(tk.END, "%s: %s\n" % (timeStamp, message))
        self.tab1.text2.see(tk.END)  # Scroll to end.

    def start_capture(self):
        # Release VirtualBox keyboard capture.
        # Doesn't seem to help though... :(
        self.client.server.key_press(key="Control_R")
        self.log("Starting capture")
        self.bind("<Any KeyPress>", lambda event: self.send_key(event.keysym))
        self.button1.config(state=tk.DISABLED)
        self.button2.config(state=tk.NORMAL)

    def stop_capture(self):
        self.log("Stopping capture")
        self.bind("<Any KeyPress>", self.dummy_event)
        self.button1.config(state=tk.NORMAL)
        self.button2.config(state=tk.DISABLED)

    def dummy_event(self, event):
        pass

    def send_key(self, key):
        if self.display_entered_text.get():
            self.tab1.text1.insert(tk.END, (key if key != 'space' else ' '))
            self.tab1.text1.see(tk.END)  # Scroll to end.
        if key in IGNORED_KEYS:
            return
        key = TRANSLATE_KEYS.get(key, key)

        with self.buffer_lock:
            if key in LITERAL_KEYS:
                self.aenea_buffer.append(key)
            else:
                self.client_proxy.write_text(''.join(self.aenea_buffer))
                self.client_proxy.key_press(key=key)
                self.aenea_buffer = []
            self.buffer_ready.notify()

    def clear_text(self):
        self.tab1.text1.delete("1.0", tk.END)

    def worker_thread(self):
        while 1:
            self.buffer_lock.acquire()

            # Wait until buffer is non-empty.
            while not self.aenea_buffer and not self.client_proxy._commands:
                self.buffer_ready.wait()

            # Grab the buffer
            text = self.aenea_buffer
            commands = self.client_proxy

            # Flush the buffer
            self.aenea_buffer = []
            self.client_proxy = communications.BatchProxy()

            self.buffer_lock.release()

            # Add text to batch buffer
            if text:
                commands.write_text(''.join(text))

            # Flush buffer. Note that RPC calls block.
            self.client.execute_batch(commands._commands)


if __name__ == "__main__":
    try:
        ip = sys.argv[1]
        port = sys.argv[2]
    except IndexError:
        ip = config.HOST
        port = config.PORT

    root = AeneaClient(ip, port)
    root.mainloop()
