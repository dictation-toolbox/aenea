# NOTE:
# For the recognition results window to work,
# in addition to placing this file in your dragonfly/macro directory
# you need to install the plugin 'recognition_results' in the Aenea server

import aenea
from dragonfly import RecognitionObserver

class AeneaRecognitionResultsObserver(RecognitionObserver):
    """Relays the recognition results to the aenea server. 
    There are three possible results: 
    * The start of a recognition
    * A successful recognition (with the recognized phrase)
    * A failed recognition
    """
    
    def on_begin(self):
        try:
            aenea.communications.server.on_begin()
        except Exception as e:
            self.print_error(e)
            
    def on_recognition(self, words):
        phrase = ' '.join(words)
        try:
            aenea.communications.server.on_recognition(phrase=phrase)
        except Exception as e:
            self.print_error(e)
        

    def on_failure(self):
        try:
            aenea.communications.server.on_failure()
        except Exception as e:
            self.print_error(e)
            
    def print_error(self,e):
        print "Exception notifying server; check that the plug-in  'recognition_results'  has been installed correctly in the server:",e
        
observer=AeneaRecognitionResultsObserver()  
observer.register()
print "AeneaRecognitionResultsObserver registered successfully"

def unload():
    global observer
    if observer : observer.unregister()
    observer = None
    
    
