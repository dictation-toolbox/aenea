import Tkinter as tk
from Tkinter import *  # @UnusedWildImport
from ttk import *  # @UnusedWildImport
import tkFont
import datetime

import communications

FLUSH_DELAY = 20 # 20 milliseconds

class AeneaClient(tk.Tk):

    def __init__(self, ip, port):
        tk.Tk.__init__(self)
        self.aenea_buffer = []
        self.last_aenea_buffer_update = 0
        self.aenea_worker_active = False
        self.wm_title("Aenea client - Dictation capturing")
        self.geometry('400x600+400+0')
        self.wait_visibility(self)
        note = Notebook(self)
        self.tab1 = Frame(note)
        self.tab2 = Frame(note)
        w = LabelFrame(self.tab1, text=u"Controls")
        w.pack(side=TOP, fill=BOTH)
        self.button1 = Button(w, text=u"Start capture",
            command=self.start_capture)
        self.button1.pack(side=LEFT)
        self.button2 = Button(w, text=u"Stop capture",
            command=self.stop_capture, state=DISABLED)
        self.button2.pack(side=LEFT)

        dFont = tkFont.Font(family="Tahoma", size=8)

        l = Label(self.tab1, text=u"Capture:")
        l.pack(side=TOP)

        self.tab1.text1 = Text(self.tab1, width=16, height=5, font=dFont)
        yscrollbar = Scrollbar(self.tab1.text1, orient=VERTICAL,
            command=self.tab1.text1.yview)
        yscrollbar.pack(side=RIGHT, fill=Y)
        self.tab1.text1["yscrollcommand"] = yscrollbar.set
        self.tab1.text1.pack(side=TOP, fill=BOTH, expand=YES)
        self.tab1.pack(side=TOP,  fill=X)
        self.tab1.text1.bind("<FocusIn>", lambda event: self.focus())

        l = Label(self.tab1, text=u"Log:")
        l.pack(side=TOP)

        self.tab1.text2 = Text(self.tab1, width=16, height=5, font=dFont)
        yscrollbar = Scrollbar(self.tab1.text2, orient=VERTICAL,
            command=self.tab1.text2.yview)
        yscrollbar.pack(side=RIGHT, fill=Y)
        self.tab1.text2["yscrollcommand"] = yscrollbar.set
        self.tab1.text2.pack(side=TOP, fill=BOTH, expand=YES)
        self.tab1.pack(side=TOP,  fill=X)
        self.tab1.text2.bind("<FocusIn>", lambda event: self.focus())

        l = Label(self.tab2, text=u"Todo...")
        l.pack(side=LEFT)

        note.add(self.tab1, text="Capturing")
        note.add(self.tab2, text="Configuration")
        note.pack(side=LEFT, fill=BOTH, expand=YES)

        try:
            self.client = communications.Proxy(ip, int(port))
            self.client_proxy = communications.BatchProxy()
        except Exception as e:
            self.log(str(e))

    def log(self, message):
        timeStamp = datetime.datetime.now()
        self.tab1.text2.insert(END, "%s: %s\n" % (timeStamp, message))
        self.tab1.text2.see(END)  # Scroll to end.

    def start_capture(self):
        # Release VirtualBox keyboard capture.
        # Doesn't seem to help though... :(
        self.client.server.key_press(key="Control_R")
        self.log("Starting capture")
        self.bind("<Any KeyPress>",
            lambda event: self.send_key(event.keysym))
        self.button1.config(state=DISABLED)
        self.button2.config(state=NORMAL)

    def stop_capture(self):
        self.log("Stopping capture")
        self.bind("<Any KeyPress>", self.foo)
        self.button1.config(state=NORMAL)
        self.button2.config(state=DISABLED)

    def foo(self, event):  # Dummy method.
        pass

    def send_key(self, key):
        self.tab1.text1.insert(END, key)
        self.tab1.text1.see(END)  # Scroll to end.
        if key in ('Shift_L', 'Control_L', 'Alt_L', '??'):
            return
        if key in translateKeys.keys():
            key = translateKeys[key]
    #         print(key)
        self.last_aenea_buffer_update = datetime.datetime.now().microsecond / 1000
        
        # TODO does this catch all cases?
        if len(key) == 1:
            self.aenea_buffer.append(key)
        else:
            self.client_proxy.write_text(''.join(self.aenea_buffer))
            self.client_proxy.key_press(key=key)
            self.aenea_buffer = []
        if not self.aenea_worker_active:
            self.aenea_worker_active = True
            self.after(FLUSH_DELAY, self.flush_buffer)
    
    def flush_buffer(self):
        delta = (datetime.datetime.now().microsecond / 1000 -
                 self.last_aenea_buffer_update)
        if delta < FLUSH_DELAY:
            self.after(FLUSH_DELAY - delta, self.flush_buffer)
        else:
            if self.aenea_buffer:
                self.client_proxy.write_text(''.join(self.aenea_buffer))
                self.aenea_buffer = []
            self.aenea_worker_active = False
            self.client.execute_batch(self.client_proxy._commands)
            self.client_proxy = communications.BatchProxy()


translateKeys = {
#     "space": "space",
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


if __name__ == "__main__":
    try:
        ip = sys.argv[1]
        port = sys.argv[2]
    except IndexError:
        ip = "192.168.0.3"
        port = 8240

    root = AeneaClient(ip, port)
    root.mainloop()
