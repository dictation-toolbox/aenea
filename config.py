# Host-side config (Linux host receiving commands)
HOST = "192.168.56.1"
PORT = 8240

# When using the Text action, grammars may request (the default is not to) to
# input the text using the clipboard rather than emulating keypresses. This has
# the advantage of allowing far more rapid entry of a large chunk of text, but
# may cause strange behavior with programs that don't understand middle click
# paste. This is implemented using xsel, meaning that after text entry xsel may
# remain running until the clipboard is cleared (this is necessary because X11
# clipboards are not buffers, they are communication protocols.). I have verified
# that there should only be at most three xsel processes running at a time,
# though they may be quite long-lived, they do not consume substantial resources.
#
# Few programs use the SECONDARY buffer, which is where we back up the PRIMARY
# buffer (middle click paste) during the operation. This buffer is clobbered.
#
# The server should clear the text it entered from the system clipboard after
# entering it, so you do not need to worry about accidentally pasting it
# somewhere else later.
ENABLE_XSEL = True
