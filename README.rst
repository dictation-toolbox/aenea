=================
Aenea
=================

A system to allow speech recognition via Dragonfly on one computer to send events to another.

| Alex Roper
| alex@aroper.net
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

Aenea is a project to allow Dragonfly (a Python-based voice macro system for Windows) to send commands to another computer. Typically, this is used to run Dragonfly in a virtual machine while commands are sent to the host operating system. Currently only Linux hosts are fully supported, but a working subset of the functionality is available for Windows. The primary audience is system administrators and software engineers.

Current Features:

- Control keyboard and mouse on one computer using your voice on another. (Supports proxy versions of Dragonfly's Key, Text, and Mouse commands)
- Enable or disable voice commands based on the currently active window on the host. (Supports flexible proxy versions of Dragonfly's AppContext)
- Access to Dragonfly's powerful grammar specification, using Dragon NaturallySpeaking (via Natlink) or Windows Speech Recognition (not tested).
- Many Dragonfly grammars will work with only minor modification via proxy.
- Dictate prose directly into any remote application via emulated keystrokes using the keystroke capture client.

Primary limitations:

- Limited ability to take advantage of context when dictating prose.
- Lacks ability to use context dependent editing commands (select that, etc)
- Requires some knowledge of Python programming to edit or create new grammars, and in some cases to customize.
- Relies on neither free nor gratis Windows and Dragon NaturallySpeaking.
- Somewhat complex to set up, depending on your background.

Missing features:

- Currently no encryption or authentication for remote control protocol (not a huge issue since it is typically used on single user systems via loopback).
- Currently only fully supports a Linux host. Partial support for Windows is available, and OS X support is in development here: https://github.com/dopey/aenea-fork.
- No actions for window management/etc on Gnome/KDE/etc. (pull requests welcome)

The primary focus of this project is writing code, system administration, terminal use, etc, and it works quite well for those tasks. For writing prose, word processing, etc., this project is quite limited compared to using Dragon natively on Windows, though it is still quite usable for those tasks.

Current Status
---------------
This project currently does everything I need well enough, and no new features are currently planned. I remain astounded by what Nuance has made possible via Natlink, and the flexibility and power of Dragonfly. I'm happy to help with troubles getting set up and take a look at bugs you may encounter, and still welcome pull requests.

Overview
--------

The system consists of a client, Dragon NaturallySpeaking with Natlink and Dragonfly and any voice grammars the user wishes to use running on a Windows virtual machine, and a server, running on the host computer. The client listens on a microphone, recognizes commands based on what you say and the current context on the server, and then sends commands to the server via JSON-RPC to perform actions such as pressing keys and clicking the mouse.

Aenea provides Proxy versions of Dragonfly actions such as Key, Text, and Mouse (ProxyKey, ProxyText, ProxyMouse), which take the same specification language as Dragonfly actions, but instead forward the action to the host to execute.

Getting Started
---------------

Windows VM Software (versions given are ones I used, others likely work too):

- Windows 7 Ultimate 32 bit
- Dragon NaturallySpeaking Premium 12
- NatLink 4.1echo
- Python 2.7.5
- pywin32-2.1.8
- dragonfly-0.6.5
- python-jsonrpclib-0.1.3
- pyparsing-2.0.1

Setup Instructions
------------------

Note: poppe1219 has other installation instructions at https://github.com/dictation-toolbox/dragonfly-scripts; take a look if you're having trouble getting everything working in the VM.

Operating system, Dragon, Natlink, and Dragonfly
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

0) Install VirtualBox.

1) Install Windows. I gave it 4 GB of RAM, two processors, and a 35 GB dynamically sized hard disk, of which it is using about 10 GB currently. While it's installing, I suggest you skim the Dragonfly documentation at https://code.google.com/p/dragonfly/

2) Install Dragon, and create your profile according to their directions. IMPORTANT: Ensure that you select BestMatchIV when creating your profile. Recent versions of Dragon default to BestMatchV, which has substantially worse performance with the sorts of grammars we will be using with Dragonfly. I and others have had problems when creating a profile where only a few seconds into the volume check a pop-up appears complaining about the microphone. To get around this, I memorized the text and continued reading while clicking okay on the dialogue as soon as it appeared. I had to read the text seven or eight times speaking in an unnaturally loud voice to get past this step. You may have to try a few times. I believe this may be a side effect of the USB microphone going through the virtual machine, and as such you may consider creating your profile on a native Windows installation and then moving it over, however I have not tried this. You may also have issues getting past the microphone quality check, as I did, however it worked just fine after that.

3) Install the other software mentioned above, and enable Natlink (by selecting GUI configuration from its start menu entry with Dragon closed). Make sure you install Python and dragonfly into paths with no spaces in them.

4) In VirtualBox's networking settings, set the network to host-only adapter so the VM can't access the network and gets a subnet. If you don't do this, you will need to modify the client and server config files to specify the correct interface to connect to.

4) Now when you start Dragon, a second small window with the title "Messages from NatLink" should pop up. If you have issues with this, take a look at the various forums that discuss using NatLink/Dragonfly on Windows.

5) You should now be able to run Natlink and Dragonfly grammars in the VM. Grammars are, by default, located in C:\\NatLink\\NatLink\\MacroSystem. NatLink will load any file named _*.py. If your grammars depend on libraries, you can place them (not starting with an _) here. Your grammars will be able to import them, but NatLink will not attempt to load them directly.

6) Test that NatLink is working correctly. Copy aenea/client/_hello_world_natlink.py to C:\\NatLink\\NatLink\\MacroSystem and restart Dragon. In the "Messages from NatLink" window, you should see 'NatLink hello world module successfully loaded. All it does is print this message:-)' printed. This means that NatLink successfully loaded your grammar.

8) Copy aenea/client/_hello_world_dragonfly.py into the MacroSystem folder, and turn your microphone off and on again. Now open Notepad (or similar) and say "test hello world grammar". The phrase 'Hello world grammar: recognition successful!' should be typed into the active window. If this doesn't work, try switching Dragon to command mode first. If it still doesn't work, try restarting Dragon. If it still doesn't work, then there is an issue with Dragon/NatLink/Dragonfly.

9) Delete the two test grammars. You're ready to move on to real ones!

Aenea Client
~~~~~~~~~~~~

These instructions are for a manual install. If you would like to have your MacroSystem directory on the VM managed from the host, see https://github.com/dictation-toolbox/gladstone. If you're not sure, you should use Gladstone.

0) Copy aenea/client/aenea to C:\\NatLink\\NatLink\\MacroSystem.

1) Copy config.py.example to config.py and edit if desired (the default config assumes the VM is using a host-only adapter which is NOT the default in VirtualBox.).

2) Copy aenea/client/_aenea_status.py to C:\\NatLink\\NatLink\\MacroSystem. While optional, this module will print information to the NatLink window when you start Dragon that can be useful for troubleshooting.

3) Turn microphone off and back on. You should see in the NatLink window 'Aenea client-side modules loaded successfully.' along with config information. If not, try restarting Dragon. It will also say it was unable to connect to the server, since we have not set that up yet.

Server (Linux X11)
~~~~~~~~~~~~~~~~~~

0) Go to aenea/server/linux_x11

1) Copy config.py.example to config.py. Edit to suit. The default assumes you are using a host-only adapter for the VM which is NOT the default. Note that the HOST/PORT here must work with those specified in the client-side config (in most cases they will need to be identical).

2) Install the dependencies. Versions I used are in parentheses; you don't need these exact versions for it to work. Install jsonrpclib (0.1.3), xdotool (3.20140213.1), and xsel (1.2.0; optional but recommended). Some window managers (xmonad) may require you to enable extended window manager hints for getcontext to work properly. On Awesome, it works out of the box.

3) Run server_x11.py. Specify -d if you want it to daemonize; default is to run in foreground.

4) In a separate terminal (or the same one if you daemonized), cd to the linux_x11 dir and run test_client.py. This should type out some text like AABB and a dict describing the context of your terminal, move the mouse around, right click and drag, etc, to test it's all working. I tried not to make it too invasive but just in case, best not have anything you care about on screen! If this works, then the server is operational and accepting commands from clients.

5) Once the server is running, copy aenea/client/_hello_world_aenea.py into MacroSystem and restart Dragon. You should see the text 'Aenea: Successfully connected to server.' in the Natlink window. Pull up a window on the Linux host and try saying 'test hello world remote grammar'. The text 'Aenea remote setup operational' should be typed into the active window on the Linux host.

Server (Windows)
~~~~~~~~~~~~~~~~

windows server by @grayjay

Note that the Windows server only supports a subset of the commands (``key_press``, ``write_text``, and ``pause``; ``get_context`` currently only returns the title of the foreground window as "title" and the title of the foreground window's ancestor as "name".).

Installation:

- Install the Haskell Platform for Windows from http://www.haskell.org/platform.
- Run the command ``cabal update``.
- Run ``cabal install`` in the folder ...aenea\\WindowsServer\\aenea-windows-server to install aenea.exe for the current user.
- aenea takes optional arguments specifying the IP address and port. These should match those on the server config.

Server (OS X)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@dopey wrote a OS X server for the older version of the communication protocol, and some work would be necessary to get it to work with the current version (pull requests welcome): https://github.com/dopey/aenea-fork

Aenea Dictation Client (optional)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Also available is a dictation capture client @poppe1219 wrote. This is simply a window that captures all keystrokes typed into it an relays them to the Linux host. If you disable Dragon's dictation box, you can dictate in Dragon's normal mode with the capture client in the foreground in Windows. Dragon will then type into the client, which will send the keystrokes to the server. You can still use grammars with the client in the foreground.

Snapshot and backup (MANDATORY)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a brittle setup. Part of why I went with a Windows VM and remote connection rather than something like Platypus is for the encapsulation. Several times, my VM has broken for no clear reason, with Dragon permacrashing, NatLink not starting, etc, and I was unable to fix it. Reverting to a snapshot easily and quickly fixed my problem, and in the year+ I've used this I've never had more than a few minutes of downtime thanks to snapshots and backups. Once you have it working, take a snapshot AND backup your VM image. You don't want to have to go through that setup process ever again. Seriously, do it now. I'll wait. Don't think of this VM as a OS, think of it as an embedded device that just does one thing.

Security
----------------

Virtual machines have a nasty tendency to not be up-to-date and at any rate they increase the attack surface. Therefore I recommend that you select "Host-only adapter" in virtual box so that the virtual machine can only connect to your computer and not to the Internet, thus limiting its potential to get compromised.

Please remember that the server simply accepts any connection made to it and will execute the commands given, and that command streams are neither authenticated nor encrypted. I hope to address this in the future, but for now I strongly suggest only running the system on a network interface you trust (i.e., VirtualBox's subnet). Be careful that other virtual machines you may run on the same system cannot access it, if you are concerned about security.

Using Aenea-Aware Modules
-------------------------

Use Gladstone, or drop them in C:\\NatLink\\NatLink\\MacroSystem\\ along with anything they depend on. Restart Dragon. You MAY be able to just turn the mic off and on again, but this doesn't always work due to how NatLink decides when to reload a module.

Using Dragonfly Modules
--------------------------

To make a dragonfly module work with Aenea, add the line::

      from aenea.proxy_nicknames import *
      
to the top of the file below the rest of the imports. This will replace Dragonfly's action and context classes with those from Aenea. Some dragonfly modules make use of actions or context features that require modification to work with Aenea, or will not work at all.

Non-exhaustive list of Dragonfly modules that should work (with the above change):

- multiedit
- cmdmemory
- kbbreak
- firefox (except save_now command)
- audacity

Writing Your Own Modules
----------------------------
Writing your own modules is quite easy and the Dragonfly documentation is extensive. This section details what you will need to know to make your modules work via a proxy.

Dragonfly classes with Proxies available (usage is identical to Dragonfly classes of same name):

- Key: press and hold/release keys. (Key names allowed are any Dragonfly allows as well as the keysyms in aenea/client/aenea/keys.txt.
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
            from aenea.proxy_nicknames import *
      except ImportError:
            pass

(This of course assumes from-style import was used to import Key, Text, etc in the original module.)

Writing Your Own Server
---------------------------

Writing your own server should be fairly straightforward. All you would need to do is implement the JSON-RPC calls from server_x11.py. The protocol as of this writing should be reasonably stable, although I do intend to add encryption and authentication support in the future, but this will likely occur via TLS.

Help!
------

Please feel free to email me if you have questions about this system or issues getting it working. I don't use it as much as I used to, but I'm still happy to discuss getting it to work and improving it, particularly the setup instructions.
