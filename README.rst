.. image:: https://travis-ci.com/dictation-toolbox/aenea.svg?branch=master
    :target: https://travis-ci.com/dictation-toolbox/aenea

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
| ...and to Joel Gould for NatLink, making it possible...
| ...and to the current maintainers of Natlink: Rudiger Wilke, Mark Lillibridge, and Quintijn Hoogenboom...
| ...and to Christo Butcher and Dragonfly for making it easy...
| ...and to Nuance for being so awesomely hack friendly...
| (even if it means we have to write the grammars ourselves :-) )

-------------------------------------------------------------------------------------------

Summary
-------

Aenea is a project to allow Dragonfly (a Python-based voice macro system for Windows) to send commands to another computer. Typically, this is used to run Dragonfly in a virtual machine while commands are sent to the host operating system. Currently only Linux and OS X hosts are fully supported, but a working subset of the functionality is available for Windows. The primary audience is system administrators and software engineers.

Current Features:

- Control keyboard and mouse on one computer using your voice on another. (Supports proxy versions of Dragonfly's Key, Text, and Mouse commands)
- Enable or disable voice commands based on the currently active window on the host. (Supports flexible proxy versions of Dragonfly's AppContext)
- Access to Dragonfly's powerful grammar specification, using Dragon NaturallySpeaking (via Natlink) or Windows Speech Recognition (not tested).
- Many Dragonfly grammars will work with only minor modification via proxy.
- Dictate prose directly into any remote application via emulated keystrokes using the keystroke capture client.
- Easily add custom Python RPCs run on the server (Linux host) that will be available from your grammars (see server/linux_x11/plugins and client/_server_plugin_example.py)
- More of a toolkit to voice-enable your current workflow than an off the shelf development environment.

Primary limitations:

- Limited ability to take advantage of context when dictating prose.
- Lacks ability to use context dependent editing commands (select that, etc)
- Requires some knowledge of Python programming to edit or create new grammars, and in some cases to customize.
- Relies on neither free nor gratis Windows and Dragon NaturallySpeaking.
- Somewhat complex to set up, depending on your background.

Missing features:

- Currently no encryption or authentication for remote control protocol (not a huge issue since it is typically used on single user systems via loopback).
- Currently only fully supports a Linux X11 and OS X host. Partial support for Windows is available.

The primary focus of this project is writing code, system administration, terminal use, etc, and it works quite well for those tasks. For writing prose, word processing, etc., this project is quite limited compared to using Dragon natively on Windows, though it is still usable for those tasks.

Current Status
---------------
This project currently does everything I need well enough, and no new features are currently planned. I remain astounded by what Nuance has made possible via Natlink, and the flexibility and power of Dragonfly. I'm happy to help with troubles getting set up and take a look at bugs you may encounter, and still welcome pull requests.

Overview
--------

The system consists of a client, Dragon NaturallySpeaking with Natlink and Dragonfly and any voice grammars the user wishes to use running on a Windows virtual machine, and a server, running on the host computer. The client listens on a microphone, recognizes commands based on what you say and the current context on the server, and then sends commands to the server via JSON-RPC to perform actions such as pressing keys and clicking the mouse.

Aenea provides Proxy versions of Dragonfly actions such as Key, Text, and Mouse (ProxyKey, ProxyText, ProxyMouse), which take the same specification language as Dragonfly actions, but instead forward the action to the host to execute. There are also wrapper classes that will respect whether or not the proxy is enabled and delegate execution to either Dragonfly (locally) or via the server. There are also wrappers that will take different actions based on whether the proxy is enabled and/or which OS is running on the server.

Getting Started
---------------

Windows VM Software (versions given are ones I used, others likely work too):

- Windows 7 Ultimate 32 bit
- Dragon NaturallySpeaking Premium 12. (Version 13 also works, with noticeably better accuracy than 12!)
- NatLink 4.1echo. 4.1whiskey also works. (look for Natlink download links here https://qh.antenna.nl/unimacro/installation/installation.html)
- Python 2.7.5
- pywin32-2.1.8
- dragonfly2-0.8.0 (please read https://github.com/Danesprite/dragonfly#installation)
- python-jsonrpclib-0.1.7
- pyparsing-2.0.1

Some notes:

- NatLink has some problems on Windows 10 64-bit related to the msvcr100.dll file. It is unclear whether it is the 64-bit or the Windows 10 which resulted in the problem. Therefore it is recommended that you use the version of Windows mentioned above.
- If you have problems installing NatLink, this page may help http://qh.antenna.nl/unimacro/installation/problemswithinstallation.html
- A previous version of these instructions recommended python-jsonrpclib-0.1.3, but I ran into a bug in it that was fixed in the later version

Setup Instructions
------------------

Note: poppe1219 has other installation instructions at https://github.com/dictation-toolbox/dragonfly-scripts; take a look if you're having trouble getting everything working in the VM.

Operating system, Dragon, Natlink, and Dragonfly
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

0) Install VirtualBox.

1) Install Windows. It works well with 1 GB of RAM, two processors, and a 35 GB dynamically-sized hard disk, of which it is uses about 17-20 GB. (You can increase the RAM to speed up the installation process, and then lower it later to spare system resources.) While it's installing, I suggest you skim the Dragonfly documentation at http://dragonfly2.readthedocs.org/en/latest/

2) Install Dragon, and create your profile according to their directions. IMPORTANT: Ensure that you select BestMatchIV when creating your profile. Recent versions of Dragon default to BestMatchV, which has substantially worse performance with the sorts of grammars we will be using with Dragonfly.

Note: I and others have had problems when creating a profile where only a few seconds into the volume check a pop-up appears complaining about the microphone. To get around this, I memorized the text and continued reading while clicking okay on the dialogue as soon as it appeared. I had to read the text seven or eight times speaking in an unnaturally loud voice to get past this step. You may have to try a few times. I believe this may be a side effect of the USB microphone going through the virtual machine, and as such you may consider creating your profile on a native Windows installation and then moving it over, however I have not tried this. You may also have issues getting past the microphone quality check, as I did, however it worked just fine after that.

3) Install the other software mentioned above, and enable Natlink (by selecting GUI configuration from its start menu entry with Dragon closed). Make sure you install Python and dragonfly into paths with no spaces in them.

4) In VirtualBox's networking settings, set the network to host-only adapter so the VM can't access the network and gets a subnet. If you don't do this, you will need to modify the client and server config files to specify the correct interface to connect to.

5) Now when you start Dragon, a second small window with the title ``Messages from NatLink`` should pop up. If you have issues with this, take a look at the various forums that discuss using NatLink/Dragonfly on Windows.

6) You should now be able to run Natlink and Dragonfly grammars in the VM. Grammars are, by default, located in ``C:\\NatLink\\NatLink\\MacroSystem``. NatLink will load any file named ``_*.py`` (where ``*`` is a wildcard). If your grammars depend on libraries, you can place them (not starting with an ``_``) here. Your grammars will be able to import them, but NatLink will not attempt to load them directly.

7) Test that NatLink is working correctly. Copy ``aenea/client/_hello_world_natlink.py`` to ``C:\\NatLink\\NatLink\\MacroSystem`` and restart Dragon. In the ``Messages from NatLink`` window, you should see ``NatLink hello world module successfully loaded. All it does is print this message:-)`` typed out into Notepad. This means that NatLink successfully loaded your grammar. You can now delete the file you just created inside ``C:\\NatLink\\NatLink\\MacroSystem`` along with its corresponding ``.pyc`` file.

9) Copy ``aenea/client/_hello_world_dragonfly.py`` into the MacroSystem folder, and turn your microphone off and on again. Now open Notepad (or similar) and say ``test hello world grammar``. The phrase ``Hello world grammar: recognition successful!`` should be typed into the active window. (If you are curious to see how it works, open the ``aenea/client/_hello_world_dragonfly.py`` file to have a look - this will be good preparation for your future grammar writing career :P). If this doesn't work, try switching Dragon to command mode first. If it still doesn't work, try restarting Dragon. If it still doesn't work, then there is an issue with the setup of Dragon/NatLink/Dragonfly. Once the ``recognition successful`` has been typed out into Notepad, you can now delete the file you just created inside ``C:\\NatLink\\NatLink\\MacroSystem`` along with its corresponding ``.pyc`` file.

10) You're ready to move on to real ones in the next section! Jump to the server section that corresponds to your host operating system.

Server (Linux X11)
~~~~~~~~~~~~~~~~~~

0) Go to ``aenea/server/linux_x11``

1) Copy ``config.py.example`` to ``config.py``. Edit to suit. The default assumes you are using a host-only adapter for the VM which is NOT the default. Note that the HOST/PORT here must work with those specified in the client-side config (in most cases they will need to be identical).

2) Install the dependencies. Versions I used are in parentheses for reference; you probably don't need these exact versions for it to work. Install ``jsonrpclib`` (0.1.7), ``xdotool`` (3.20140213.1), ``xprop`` (1.2.3), ``xsel`` (1.2.0; optional but recommended), and ``yapsy`` (1.10.223-1; optional but recommended if you want server-side plugin support). Some window managers (``xmonad``) may require you to enable extended window manager hints for getcontext to work properly. On Awesome, it works out of the box.

3) Edit the server's ``config.py.example`` to specify the host and port it should listen on.

4) Run ``server_x11.py``. Specify -d if you want it to daemonize; default is to run in foreground.

5) In a separate terminal (or the same one if you daemonized), ``cd`` to the ``linux_x11`` dir and run ``test_client.py``. This should type out some text like ``AABB`` and a dict describing the context of your terminal, move the mouse around, right click and drag, etc, to test it's all working. I tried not to make it too invasive but just in case, best not have anything you care about on screen! If this works, then the server is operational and accepting commands from clients. No point trying to get it to work with Dragon and the VM until it can accept local commands!

Server (Windows)
~~~~~~~~~~~~~~~~

Note that the windows server does not support a security token, without which any webpage can POST RPCs to the server just by getting you to click a link.

windows server by @grayjay

Note that the Windows server only supports a subset of the commands (``key_press``, ``write_text``, and ``pause``; ``get_context`` currently only returns the title of the foreground window as "title" and the title of the foreground window's ancestor as "name".).

Installation:

- Install the Haskell Platform for Windows from http://www.haskell.org/platform.
- Run the command ``cabal update``.
- Run ``cabal install`` in the folder ...``aenea\\WindowsServer\\aenea-windows-server`` to create aenea.exe in cabal's bin folder.
- aenea.exe takes optional arguments specifying the IP address and port. These should match those on ``C:\\NatLink\\NatLink\\MacroSystem\\aenea.json``.
- Set use_multiple_actions to false in aenea.json.

Server (OS X)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Install:
  - ``python2``
  - ``pip install pyobjc``   (this is required for py-applescript and will take a while.  no, a really, really long while)
  - ``pip install py-applescript``

Enable access for assistive devices in your system preferences


Aenea client-side library
~~~~~~~~~~~~
At this point, the folder ``C:\\NatLink\\NatLink\\MacroSystem`` should contain a folder named core (which would have been created after installing and enabling Natlink).

0) Close Dragon and then copy ``aenea/client/aenea`` into ``C:\\NatLink\\NatLink\\MacroSystem``.

1) Copy ``aenea/aenea.json.example`` into ``C:\\NatLink\\NatLink\\MacroSystem``. Rename it to ``aenea.json`` and edit to suit.

1a) For aenea itself you have a choice -- you can either store its state and configuration files (these are used for keeping track of which dynamic vocabulary are currently active, which server to send commands to, etc) in ``C:\\Natlink\\NatLink\\MacroSystem``, or you can store them elsewhere. If you store them in ``MacroSystem`` just edit ``aenea.json`` to suit and you're done. If you want to store it elsewhere (I put it on a shared folder mounted as the ``E`` drive so I can manage it from the host), then delete all the lines except ``project_root``', and set its value to whatever directory you want to manage the config from. Then, in that directory, copy the full ``aenea.json.example`` and edit to taste. Basically on startup we first load ``C:\\NatLink\\NatLink\\MacroSystem\\aenea.json`` (hardcoded), then if the ``project_root`` specified is another directory we load ``aenea.json`` from that directory, overwriting any settings, and repeat until ``aenea.json`` specifies its own path (or a cycle which is an error). All other config files are relative to the ``project_root``.

1b) If not using VirtualBox host only adapter as described above, you will need to set the host and port to the correct settings.

4) Copy ``aenea/client/_hello_world_aenea.py`` into ``C:\\NatLink\\NatLink\\MacroSystem``, and restart Dragon. Now try saying ``test hello world remote grammar``. The text ``Aenea remote setup operational`` should be typed through the server, into whatever window is in the foreground (unless it is the VM itself). The server will also print updates for every command received and executed to aid in debugging setup issues. If it doesn't work, check the NatLink window for backtraces as well. Note that the JSON-RPC library will serialize and return Python exceptions from the server to print in the NatLink window, so a backtrace in that window can be either from the client or the server.

5) If all's well, delete ``_hello_world_aenea.py`` from ``MacroSystem``.

Built-In Optional Modules
~~~~~~~~~~~~~~~~~~~~~~~~~

While optional, Aenea comes with two very useful modules.

``_aenea.py`` allows you to dynamically switch between local (i.e., in the VM) and remote (i.e., send to server), as well as changing which server commands are sent to (if you're using several different computers). It will also print useful information when the module is loaded such as the current networking settings. To install, just copy ``client/_aenea.py`` into the MacroSystem directory. It is configured in ``ROOT\\grammar_config\\aenea.json``, there you can rebind commands and add or remove servers to connect to. It reads and writes ``ROOT\\server_state.json`` to keep track of which server is currently active.

``_vocabulary.py`` is used by most of my grammars, and allows multiple grammars to make use of the same set of vocabulary. (For example, one may want access to Python vocabulary both in a VIM grammar and a generic edit grammar). It makes use of ``ROOT\\vocabulary_config``. ``ROOT\\vocabulary_config\\static`` contains vocabularies that are always enabled, and ``ROOT\\vocabulary_config\\dynamic`` contains vocabularies that may be switched on and off by the user at will. ``ROOT\\vocabulary_config\\enabled.json`` (read and written) keeps track of the current state of dynamic vocabularies. You can rebind the commands used to control vocabulary in ``ROOT\\grammar_config\\vocabulary.json``. To install, just copy ``client/_vocabulary.py`` into the ``MacroSystem`` dir.

Aenea Dictation Client (optional)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Also available is a dictation capture client @poppe1219 wrote. This is simply a window that captures all keystrokes typed into it an relays them to the Linux host. If you disable Dragon's dictation box, you can dictate in Dragon's normal mode with the capture client in the foreground in Windows. Dragon will then type into the client, which will send the keystrokes to the server. You can still use grammars with the client in the foreground. To use, just copy ``client/aenea_client.py`` to ``MacroSystem`` and run it. By default, all grammars will only work when the client is in the foreground. You can change this behavior in ``aenea.json`` by setting ``restrict_proxy_to_aenea_client`` to ``false``.

Snapshot and backup (MANDATORY)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a brittle setup. Part of why I went with a Windows VM and remote connection rather than something like Platypus and/or wine is for the encapsulation. Several times, my VM has broken for no clear reason, with Dragon permacrashing, NatLink not starting, etc, and I was unable to fix it. Reverting to a snapshot easily and quickly fixed my problem, and in the year+ I've used this I've never had more than a few minutes of downtime thanks to snapshots and backups. Once you have it working, take a snapshot AND backup your VM image. You don't want to have to go through that setup process ever again. Seriously, do it now. I'll wait. Don't think of this VM as a OS, think of it as an embedded device that just does one thing.

Security
----------------

Virtual machines have a nasty tendency to not be up-to-date and at any rate they increase the attack surface. Therefore I recommend that you select ``Host-only adapter`` in virtual box so that the virtual machine can only connect to your computer and not to the Internet, thus limiting its potential to get compromised.

Please remember that the server simply accepts any connection made to it and will execute the commands given, that command streams are neither authenticated nor encrypted, and that the server is not written to deal with untrusted clients. I hope to address authentication and encryption in the future (I see little point to dealing with untrusted clients given they literally control your computer), but for now I strongly suggest only running the system on a network interface you trust (i.e., VirtualBox's subnet). Be careful that other virtual machines you may run on the same system cannot access it, if you are concerned about security.

Using Aenea-Aware Modules
-------------------------

Drop them in ``C:\\NatLink\\NatLink\\MacroSystem`` along with anything they depend on. In theory you can just say ``force natlink to reload all grammars`` (if you are using the ``_aenea.py`` module mentioned further above), but if anything goes wrong just restart Dragon.

Using Dragonfly Modules
--------------------------

To make a dragonfly module work with Aenea, add the line::

      from aenea.strict import *

to the top of the file below the rest of the imports. This will replace Dragonfly's action and context classes with those from Aenea. Some dragonfly modules make use of actions or context features that require modification to work with Aenea, or will not work at all. This of course assumes * import style was used for dragonfly in the module.

Non-exhaustive list of Dragonfly modules that should work (with the above change):

- multiedit
- cmdmemory
- kbbreak
- firefox (except save_now command)
- audacity

Writing Your Own Modules
----------------------------
Writing your own modules is quite easy and the Dragonfly documentation is extensive. This section details what you will need to know to make your modules work via a proxy, and does not duplicate the Dragonfly documentation.

Aenea provides several classes which take an action via the proxy server. Their class names start with Proxy:

- ProxyAppContext -- provides an AppContext that lets you match on the title, window class/window class name, etc of the currently active window on the host. This tries to be a drop-in replacement for AppContext, but can't quite work the same way since we need to take X11 properties into account.
- ProxyCustomAppContext -- provides a custom context that allows querying by any value the server provides. See the docstring for details.
- ProxyCrossPlatformContext -- chooses between one of several contexts based on what OS the server reports is running. Pass in a dict-like from OS to Context. Note that the OS is queried dynamically -- whenever we use the context, so you can use this if you need to switch between servers.
- ProxyPlatformContext -- chooses between one of two contexts based on whether or not we are currently sending commands to the proxy server -- so you can use the same grammar on the VM/local machine and via proxy.
- ProxyKey, ProxyMouse, ProxyText -- very similar to Dragonfly's, but support additional functionality (e.g., the Key can accept Linux keysyms as well as Dragonfly ones). See their docstrings for details.
- ProxyMousePhantomClick -- Move mouse to a location, click, return. From the user's perspective, click without moving the mouse.

Additionally, there are two wrapper layers to make it easier to write a grammar that works both locally and via proxy -- aenea.lax and aenea.strict. They are identical except in how they handle errors. Strict (default) is useful when you want to write one grammar that works both locally and remotely. When the grammar is loaded, it creates a Dragonfly and Proxy object (for each OS if appropriate), and if any errors occur, it raises.

The lax version will ignore errors at grammar load time and only raise them if you attempt to actually use an invalid object. So for example, if you have a Key object press a Linux keysym, it will only error if you attempt to execute the action on the local host. If you used the strict version, your grammar would be prevented from loading:

- AeneaAction -- performs one of two actions based on whether the proxy server is currently enabled.
- AeneaContext -- uses one of two contexts based on whether the proxy server is currently enabled.
- AlwaysContext, NeverContext, NoAction -- useful for combining actions/contexts -- support combinator operators but do nothing.
- ContextAction -- takes a different action based on which context is currently active. Takes a list of (context, action) pairs. Whenever executed, all actions whose context matches are executed.
- Key, Text, Mouse -- Executes either on proxy or locally based on whether proxy server is currently enabled.

Taking advantage of the vocabulary system
-----------------------------------------

I noticed that many of my grammars had similar vocabulary but wanted to put them in different places, leading to duplication. In particular, both vim and multiedit should be usable for programming, and as such duplicated a great deal of both language specific vocabulary as well as general help. Since both of these grammars make use of nested trees, and chaining commands together in the grammar, I wanted to separate vocabulary and grammar.

Inspired by the dynamics system @nirvdrum wrote, I also wanted the ability to dynamically disable and enable certain vocabulary as appropriate (e.g., disable Python vocabulary when not using Python). The vocabulary system allows you to define vocabulary items that grammars can then hook into. Currently, multiedit, vim, and _vocabulary use them.

There are two types of vocabulary, due to Dragonfly/NatLink limitations. Static vocabularies are loaded at system start, cannot be dynamically enabled/disabled, and you need to restart Dragon to reload them. On the plus side, they can use more complex specifications such as "reload [all] (configuration|config)".

Dynamic vocabulary is limited to straight key-value pairs -- what you say and what is typed. However _vocabulary.py lets you dynamically turn them on/off as necessary.

Writing a Vocabulary
--------------------

The format is identical for both static and dynamic vocabularies. You create a JSON file in ROOT/vocabulary_config/static or ROOT/vocabulary_config/dynamic, containing several properties. "name" is what you will say to enable/disable the grammar. "tags" is a list of tags, explained below. "shortcuts" is a mapping from what you say to what KEY(s) are pressed (i.e., the string is used as the spec for a Key object). "vocabulary" is a mapping from what you say to what you get.

In addition to plain text, the value may also specify Text, Key, and Mouse actions (see the end of python.json for an example of this).

Using Vocabularies in your Grammar
----------------------------------

Vocabularies are attached to grammars by use of the tag system. Your grammars may request one or more tags, which are simply hooks vocabularies can attach to. So for example, multiedit creates "multiedit" and "multiedit.count" hooks, which are simply things which may be chained together. The .count hook means you can say a number after it to do it N times. The dynamic Eclipse vocabulary is a good example of this. For example, my Python vocabulary says it should be active in "vim.insertions.code", "multiedit", and "global". This is best explained by examining the example vocabularies at https://github.com/dictation-toolbox/aenea-grammars/tree/master/vocabulary_config.

The "global" tag is special -- it's used by _vocabulary.py for things you should be able to say anywhere. The reason it's a special case is because we want to make sure that there aren't multiple grammars competing to recognize an entry. Thus, a grammar may suppress a tag in the global context (multiedit and vim do this), so that whenever they are in use, _vocabulary won't recognize the tags they've taken over. See multiedit and vim for examples of this.

The whole system can sound quite intimidating at first (much like Dragonfly) but it's not as bad as it sounds to use, I promise! Just take a look at the example grammars and vocabularies and you'll be writing your own in no time! (example grammars: https://github.com/dictation-toolbox/aenea-grammars)

Grammar Configuration
---------------------

configuration.py is designed to provide easy to use code for grammars to read config files under PROJECT_ROOT/grammar_config. In particular, they make it easy for a grammar to allow users to overwrite their keybindings. This is similar to the idea behind Dragonfly's configuration system, but simpler and less powerful -- you can't include arbitrary code. Grammars need not use this system, but all mine do.

Documentation
-------------

The API and core are extensively documented via pydoc. I tried to provide a high level description of how it all fits together in this README, but for the latest/details, see the pydoc. aenea should import on Linux even though Dragonfly isn't there (necessary for running tests), so you should be able to browse/read the docs.

Server Plugins
--------------

You can add custom RPCs to the server using the plugin system (using yapsy). Take a look at the example plugin and corresponding grammar for details.

Writing Your Own Server
---------------------------

Writing your own server should be fairly straightforward. All you would need to do is implement the JSON-RPC calls from server_x11.py. The protocol as of this writing should be reasonably stable, although I do intend to add encryption and authentication support in the future, but this will likely occur via TLS.

Help!
------

Please feel free to post in the Dragonfly Google group https://groups.google.com/forum/#!forum/dragonflyspeech or to email me if you have questions about this system or issues getting it working. I don't use it as much as I used to, but I'm still happy to discuss getting it to work and improving it, particularly the setup instructions, and I've learned a great deal from other users already.
