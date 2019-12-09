#!/usr/bin/env python2


""" 
Tkinker application to display the last result of the recognition.
It reads the results from a text file.
The application consists of as simple label to display the results.
The file config.py contains the configuration of the appearance and location of the text file.


"""

from Tkinter import Tk, Frame, StringVar, Label, Button
import sys
import time
import Queue,threading
import config
import logging

class Application(object):
    
    def update_phrase(self,event):
        phrase = self.queue.get()
        self.set_phrase(phrase)

    def createWidgets(self):
        self.root = Tk(className="recognitionbar")
        
        self.root.wm_attributes('-type', config.window_type)
        if config.always_on_top:
            self.root.wm_attributes('-topmost', True)
        
        self.frame=Frame(master=self.root)
        self.frame.pack()
        
        self.phrase = StringVar(self.frame)
        self.phrase.set("Waiting for speech...")
        self.label = Label(self.frame,textvariable=self.phrase,font=(config.font_family, config.font_size))
        self.label["fg"]=config.foreground_color 
        self.label["bg"]=config.background_color 
        self.label.pack(anchor='center')
        
        #self.update = Button(self.frame)
        #self.update["text"] = "update"        
        #self.update["command"] =  self.update_phrase
        #self.update.pack({"side": "left"})
        if config.include_quit_button: 
            self.quit = Button(self.frame)
            self.quit["text"] = "Quit"
            self.quit["fg"]   = "red"
            self.quit["command"] =  self.frame.quit
            self.quit.pack({"side": "right"})
        
    def setup_watcher_thread(self):
        self.queue=Queue.Queue()
        self.watcher_thread=WatcherThread(self.callback)
        self.watcher_thread.daemon = True
        self.watcher_thread.start()
        
    def __init__(self):
        self.createWidgets()
        self.root.bind("<Destroy>", self.shutdown)
        self.root.bind("<<on_recognition>>", self.update_phrase)
        self.root.protocol("WM_DELETE_WINDOW", self.shutdown)
        self.setup_watcher_thread()
        
    def shutdown(self,unused_arg=None):
        sys.exit()
    
    def callback(self,phrase):
        self.queue.put(phrase)
        self.root.event_generate("<<on_recognition>>")
        
    def set_phrase(self,phrase):
        self.phrase.set(phrase)
    
        
    def mainloop(self):
        """Start the GUI loop"""
        self.root.mainloop()
    
        
class WatcherThread(threading.Thread):
    def __init__(self,callback,sleep_between=0.1):
        threading.Thread.__init__(self)
        self.callback=callback
        self.sleep_between=sleep_between
        
    def run(self):
        log=logging.getLogger("recognitionbar")
        log.info("watcher thread started")
        with open( config.results_file, "r") as pipein:       
            pipein.seek(0,2)
            while True:
                curr_position = pipein.tell()
                line = pipein.readline()
                if not line:
                    pipein.seek(curr_position)
                    time.sleep(self.sleep_between)
                else:
                    self.callback(line)
                    

import signal

def sigint_handler(sig, frame):
    app.root.quit()
    app.root.update()
    sys.exit()

# Set signal before starting
signal.signal(signal.SIGINT, sigint_handler)
logging.getLogger("recognitionbar").setLevel(logging.INFO)

app = Application()
app.mainloop()




