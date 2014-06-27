=================
Aenea
=================

A system to allow speech recognition via Dragonfly on one computer to send events to another.

| Alex Roper
| aroper@umich.edu
| http://github.com/calmofthestorm

-------------------------------------------------------------------------------------------

| With many thanks to Tavis Rudd for showing us it was practical for coding...
| (http://ergoemacs.org/emacs/using_voice_to_code.html)
| ...and to Joel Gould and NatLink for making it possible...
| ...and to Christo Butcher and Dragonfly for making it easy...
| ...and to Nuance for being so awesomly hack friendly...
| (even if it means we have to write the grammars ourselves :-) )

-------------------------------------------------------------------------------------------

Summary
-------

Aenea is a project to allow Dragonfly (a Python-based voice macro system for Windows) to send commands to another computer. Typically, this is used to run Dragonfly in a virtual machine while commands are sent to the host operating system. Currently only Linux hosts are fully supported, but a working subset of the functionality is available for Windows, and work is underway to support OS X. The primary audience is system administrators and software engineers.

Current Features:

- Control keyboard and mouse on one computer using your voice on another. (Supports proxy versions of Dragonfly's Key, Text, and Mouse commands)
- Enable or disable voice commands based on the currently active window on the host. (Supports flexible proxy versions of Dragonfly's AppContext)
- Access to Dragonfly's powerful grammar specification, using Dragon NaturallySpeaking (via Natlink) or Windows Speech Recognition.
- Many Dragonfly modules will work with only minor modification via proxy.
- Includes a detailed module for using VIM, and a customized version of the Dragonfly multiedit module designed for programming.
- Dictate prose directly into any remote application via emulated keystrokes using the keystroke capture client.

Primary limitations:

- Limited ability to take advantage of context when dictating prose.
- Lacks ability to use context dependent editing commands (select that, etc)
- Requires some knowledge of Python programming to edit or create new modules, and in some cases to customize.
- Relies on neither free nor gratis Windows and Dragon NaturallySpeaking.
- Somewhat complex to set up, depending on your background.

Missing features:

- Currently no encryption or authentication for remote control protocol (not a huge issue since it is typically used on single user systems via loopback).
- Currently only fully supports a Linux host. Partial support for Windows is available, and OS X support is in development here: https://github.com/dopey/aenea-fork.
- No actions for window management/etc on Gnome/KDE/etc. (pull requests welcome)

The primary focus of this project is writing code, system administration, terminal use, etc, and it works quite well for those tasks. For writing prose, word processing, etc., this project is quite limited compared to using Dragon natively on Windows.

Existing Modules:

- Chromium browser
- Awesome Window Manager
- Basic shell use (needs work)
- Thunderbird/Icedove
- VIM (module is named verbal_emacs.py)
- Multiedit text editing (expanded version of Christo Butcher's multiedit)

Current Status
---------------
This project currently does everything I need well enough, and no new features are currently planned. I remain astounded by what Nuance has made possible via Natlink, and the flexibility and power of Dragonfly. I'm happy to help with troubles getting set up and take a look at bugs you may encounter, and still welcome pull requests.

Overview
--------

The system consists of a client, Dragon NaturallySpeaking with Natlink and Dragonfly and any voice modules the user wishes to use running on a Windows virtual machine, and a server, running on the host computer. The client listens on a microphone, recognizes commands based on what you say and the current context on the server, and then sends commands to the server via JSON-RPC to perform actions such as pressing keys and clicking the mouse.

Aenea provides Proxy versions of Dragonfly actions such as Key, Text, and Mouse (ProxyKey, ProxyText, ProxyMouse), which take the same specification language as Dragonfly actions, but instead forward the action to the host to execute.

Getting Started
---------------

Note: poppe1219 has other installation instructions at https://github.com/poppe1219/dragonfly-scripts; take a look if you're having trouble getting everything working in the VM.

Windows VM Software (versions given are ones I used, others likely work too):

- Windows 7 Ultimate 32 bit
- Dragon NaturallySpeaking Premium 12
- NatLink 4.1echo
- Python 2.7.5
- pywin32-2.1.8
- dragonfly-0.6.5
- python-jsonrpclib-0.1.3
- pyparsing-2.0.1

Setup instructions:

0) Install VirtualBox.

1) Install Windows. I gave it 4 GB of RAM, two processors, and a 35 GB dynamically sized hard disk, of which it is using about 10 GB currently. While it's installing, I suggest you skim the Dragonfly documentation at https://code.google.com/p/dragonfly/

2) Install Dragon, and create your profile according to their directions. IMPORTANT: Ensure that you select BestMatchIV when creating your profile. Recent versions of Dragon default to BestMatchV, which has substantially worse performance with the sorts of grammars we will be using with Dragonfly. I and others have had problems when creating a profile where only a few seconds into the volume check a pop-up appears complaining about the microphone. To get around this, I memorized the text and continued reading while clicking okay on the dialogue as soon as it appeared. I had to read the text seven or eight times speaking in an unnaturally loud voice to get past this step. You may have to try a few times. I believe this may be a side effect of the USB microphone going through the virtual machine, and as such you may consider creating your profile on a native Windows installation and then moving it over, however I have not tried this. You may also have issues getting past the microphone quality check, as I did, however it worked just fine after that.

3) Install the other software mentioned above, and enable Natlink (by selecting GUI configuration from its start menu entry with Dragon closed). Make sure you install Python and dragonfly into paths with no spaces in them.

4) Now when you start Dragon, a second small window with the title "Messages from NatLink" should pop up. If you have issues with this, take a look at the various forums that discuss using NatLink/Dragonfly on Windows.

5) By default no grammars are loaded. You will find the grammars that are available in grammars_available. Copy/symlink the ones you want to use into grammars_enabled. If you're not sure which ones you want to use, all of them is a good place to start. Note that verbal_emacs relies on both verbal_emacs.py and verbal_emacs (directory).

6) Copy config.py.example to config.py and util/config.py.example to util/config.py. These are server and client config, respectively. Edit to taste.

7) I prefer to keep the primary version of my modules on the host, and use the reloadconfig module to transfer them over. In order for this to work, I set up a shared folder with the virtual machine bound to the E Drive, and put a symbolic link to aenea's folder under it, so E:\\aenea contains the code. Copy aenea/util/config.py and aenea/util/reloadconfig.py to C:\\NatLinx\\NatLinx\\MacroSystem, and then run reload-aenea-configuration.py on the Windows machine. You should see a bunch of files appear in the MacroSystem folder. With this setup, you should be able to run the script reload-aenea-configuration.py on Windows to copy over the modules in aenea/grammars_enabled whenever you update them. If you do not wish to use this functionality, you will need to copy over the modules from aenea/grammar that you wish to use yourself, renaming them so that they each start with an underscore. You will also need to copy over all the files in aenea/util. On my install, NatLink modules go in C:\\NatLink\\NatLink\\MacroSystem. Note that using reloadconfig will NOT delete files in the dest that were deleted in the source -- to remove a file, you will need to delete it on both src and dest. (This is a sanity check to prevent potential data loss related to other Natlink grammars). Note that you will need to restart Dragon after each time you reload the config (https://github.com/calmofthestorm/aenea/issues/17).

8) On the Linux host, install jsonrpclib (0.1.3), xdotool (3.20140213.1), and xsel (1.2.0; optional but recommended). If necessary, adjust the networking settings in aenea/config.py (server) and aenea/util/config.py (client). Some window managers (xmonad) may require you to enable extended window manager hints for getcontext to work properly. On Awesome, it works out of the box. Note that you don't need these exact versions; they are the ones I'm currently using for reference.

9) Run server_x11.py to listen for commands and execute them. Note that the grammars that ship with aenea will only be active if Notepad or the aenea client is open and selected in the VirtualMachine (see aenea/util/aenea.py to change this). A simple test is saying "zip 0 0", which should move the mouse to the upper left of your screen.

10) If you would like to capture normal mode dictation, you can run the experimental client that works by capturing all keystrokes typed into its window and transmitting them to the server. It is in aenea/util/aenea_client.py. Regular commands should continue to work as normal.

11) Please let me know if you encounter issues with this setup, especially if you manage to get it working -- I hope to update this document with advice to make the setup easier for future users.

Security
----------------

Virtual machines have a nasty tendency to not be up-to-date and at any rate they increase the attack surface. Therefore I recommend that you select "Host-only adapter" in virtual box so that the virtual machine can only connect to your computer and not to the Internet, thus limiting its potential to get compromised.

Please remember that the server simply accepts any connection made to it and will execute the commands given, and that command streams are not encrypted. I hope to address this in the future, but for now I strongly suggest only running the system on a network you trust (i.e., VirtualBox's subnet). Be careful that other virtual machines you may run on the same system cannot access it, if you are concerned about security.

Performance
---------------

Since regular dictation and built in Dragon commands will go to the virtual machine instead of your computer, I recommend leaving Dragon in command mode. This will substantially improve both speed and accuracy of recognition. You can use the aenea client to capture normal mode dictation and transmit it to the server.

Having more grammars and especially complex grammars will adversely affect recognition performance.

Your computer's serial processing speed is also quite important to recognition performance, as is how much RAM it has available. The quality of your microphone can also improve performance accuracy and speed substantially.

Hardware
-------------
My current hardware setup consists of the following:

- Audix OM-7 microphone (http://www.amazon.com/Audix-OM7-OM-7-Microphone/dp/B0002H0M7W)
- ART USB Pre preamp (http://www.amazon.com/Art-ART-USB-Dual-Pre/dp/B002KEAT78)
- XLR cable (to connect preamp to microphone).
- 5/8" Microphone stand

The reason I recommend this preamp is because it is a fairly cheap way to add +48 dB. I have found it necessary to use all of this at times, especially in open office settings where dictating quietly is crucial to your coworkers productivity. It is also convenient to have a hardware gain gnob (I raise the gain when no one else is talking, and lower it and dictate more loudly when others hold conversations nearby). The Audix OM-7 has quite low gain, and although amplifying in software in Audacity with a weaker preamp produces clear audio, Dragon will refuse to work with it if it has too low a volume even if the signal-to-noise ratio is quite sufficient.

Although my current hardware setup really is much better than what I started with, if you are able to talk reasonably loudly I found a cheap $10 headset worked reasonably well, and it is what I used for the first week or two. The above hardware is quite nice, but it's far from necessary to try out dictation and even do serious work with it.

I strongly suggest a USB microphone -- you can use VirtualBox to pass the USB directly through so Linux compatibility should not be an issue, and USB 1.0 should be fine for most audio equipment (which works with VirtualBox OSE). That said, my original headset connected via the microphone in and did work.

Using Dragonfly Modules
--------------------------

To make a dragonfly module work with Aenea, add the line::

      from proxy_nicknames import *
      
to the top of the file below the rest of the imports. This will replace Dragonfly's action and context classes with those from Aenea. Some dragonfly modules make use of actions or context features that require modification to work with Aenea, or will not work at all.

Non-exhaustive list of Dragonfly modules that should work (with the above change):

- multiedit
- cmdmemory
- kbbreak
- firefox (except save_now command)
- audacity

Windows Server
----------------------------
grayjay (grayjay@wordroute.com) implemented a windows server (available in WindowsServer) that supports some of the commands, including ``key_press``, ``write_text``, and ``pause``. ``get_context`` currently only returns the title of the foreground window as "title" and the title of the foreground window's ancestor as "name".

Installation:

- Install the Haskell Platform for Windows from http://www.haskell.org/platform .
- Run the command ``cabal update``.
- Run ``cabal install`` in the folder ...aenea\\WindowsServer\\aenea-windows-server to install aenea.exe for the current user.
- aenea takes optional arguments specifying the IP address and port.

OS X Server
----------------------------
dopey wrote a OS X server for the older version of the communication protocol, and some work would be necessary to get it to work with the current version (pull requests welcome): https://github.com/dopey/aenea-fork

Writing Your Own Modules
----------------------------
Writing your own modules is quite easy and the Dragonfly documentation is quite extensive. This section details what you will need to know to make your modules work via a proxy.

Dragonfly classes with Proxies available (usage is identical to Dragonfly classes of same name):

- Key: press and hold/release keys. (Key names allowed are any Dragonfly allows as well as the keysyms in aenea/util/keys.txt.
- Text: Enter a string exactly as written.
- Mouse: Click, move, and drag the mouse.

Aenea classes that work differently from Dragonfly or are not present. See their python doc strings for usage details (in aenea/util/proxy_actions.py and aenea/util/proxy_contexts.py):

- AppContext: control when a rule or grammar is active. Eg, AppContext(title="Kate") would specify to only be active when a window title containing Kate is selected. You may also specify cls, cls_name, and executable to be more precise. You can use the program xprop to find the window class and window class name of the active window.
- AppCustomContext: allows extreme flexibility in specifying precisely when a rule should be active based on the context. Supports case sensitivity, regular expressions, and querying on many more fields. Run "python server_x11.py getcontext" to show all keys available for querying defined by the active window (eg, "sleep 1s && python server_x11.py getcontext" to wait one second so you can select the window of interest).
- AlwaysContext: Always matches (useful for a starting point when using | and & on contexts).
- NeverContext: Never matches.
- NoAction: Do nothing.
- ContextAction: Perform a different action based on which context is active.
- MousePhantomClick: click the mouse at the specified coordinates and restore its previous position afterwards. (To a user, this looks like clicking a location without moving the mouse.)

It is straightforward to write a module which will work both with Aenea and with unmodified Dragonfly. To do so, simply use::

      try:
            from proxy_nicknames import *
      except ImportError:
            pass

(This of course assumes from-style import was used to import Key, Text, etc in the original module.)

My modules respect the PLATFORM variable in aenea/util/config.py rather than relying on the above.

Writing Your Own Server
---------------------------

Writing your own server should be fairly straightforward. I have spoken to others interested in writing servers for Windows and for OS X. All you would need to do is implement the JSON-RPC calls from server_x11.py. The protocol as of this writing should be reasonably stable, although I do intend to add encryption and authentication support in the future, but this will likely occur via TLS. I am very interested in accepting pull request that implement servers for other platforms, but am unlikely to write them myself.

Using My Modules Without Proxy
-------------------------------

Many of my modules will work on Windows. Only multiedit has been tested working, but these modules either should work already or will work with only minor modification. If you wish to use one of them on Windows, I would be happy to make the necessary modifications, or to accept a pull request doing so.::

- chromium (issue is figuring out context)
- multiedit (tested working)
- nato
- shell
- stopgap
- thunderbird
- verbal_emacs
- vim

Help!
------

Please feel free to email me if you have questions about this system, getting it working, customizing it, or anything else related to programming by voice. I use the system every day and although I do not have any other major changes planned as it does largely what I want, I am always open to improving it.
