
"""configuration for the plug-in and UI """

import os

##########################################
####### PLUG-IN configuration ############
##########################################
enabled = False


# path to file where the recognition results are stored
results_file = os.path.expanduser("~/.aenea_phrases.log")
# literal to display when a recognition has begun
begin_literal = "..."
# literal to display when a recognition has failed
failure_literal = "???"

##########################################
#######    UI configuration   ############
##########################################

# include a button to quit the application
include_quit_button = False

#appearance of the results
foreground_color = "black"
background_color = "#e6e195"
font_size = 12
font_family = "Fira Sans" # will resort to default if font not found

# window type: see https://www.tcl-lang.org/man/tcl/TkCmd/wm.htm#M19
window_type = 'dock' 
# window should be always on top of other applications
always_on_top = True
