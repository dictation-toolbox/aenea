# HOW TO
# This plugin users a tk python application to show a small recognition results window in the same fashion as other speech recognition systems allow.
# The file _recognition_results_observer.py must be installed in your dragonfly client folder.
# Note that the plugin uses a local file in the server to communicate the results to the recognition window. This file stores in plain text the results of your speech. If security or privacy of what you input is a concern, you should delete this file regularly and/or take the appropiate measures. By default, the file is `~/.aenea_phrases.log`.
# The recognitionbar_tk.py allows customization of its appeareance via the file `config.py`.



from yapsy.IPlugin import IPlugin
import os
import config
from subprocess import Popen

from __future__ import print_function 

enabled = True

class RecognitionResultPlugin(IPlugin):
    def __init__(self):
        self.fd = open( config.results_file, mode="at")
        current_folder = os.path.dirname(os.path.abspath(__file__))
        ui_path = os.path.join(current_folder,"recognitionbar_tk.py")
        try:
            Popen([ui_path])
        
        except Exception as e:
            print("Error writing recognition result: "+str(e))
        
        
    def register_rpcs(self, server):
        server.register_function(self.on_recognition)
        server.register_function(self.on_failure)
        server.register_function(self.on_begin)
    
    def on_begin(self,):
        self.write_message(config.begin_literal)
    def on_failure(self,):
        self.write_message(config.failure_literal)    
    def on_recognition(self,phrase="nothing"):
        self.write_message(phrase)
        
    def write_message(self,message):
        try:
            self.fd.write(message+"\n")
            self.fd.flush()
            
        except Exception as e:
            print("Error writing recognition result: "+str(e))
