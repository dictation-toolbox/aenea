How to set up a Dragon NaturallySpeaking Windows VM for use on a Linux host

-------------------------------------------------------------------------------------------

Alex Roper
aroper@umich.edu
http://github.com/calmofthestorm

-------------------------------------------------------------------------------------------

With many thanks to Tavis Rudd for showing us it was practical for coding...
  (http://ergoemacs.org/emacs/using_voice_to_code.html)
...and to Joel Gould and NatLink for making it possible...
...and to Christo Butcher and Dragonfly for making it easy...
...and to Nuance for being so awesomly hack friendly...
   (even if it means we have to write the context free grammars ourselves :-) )

-------------------------------------------------------------------------------------------

Summary
-------

The code currently is extermly poorly written. I would normally not consider releasing code
in this shape, but I needed to bootstrap a voice system before rewriting the whole thing
in Twisted and redoing the NatSpeak modules with what I now know, and wanted it to be available to others in the meantime.

1) Get a Virtual Machine and install Windows and Dragon.
2) Install a bunch of free software on the VM. (Included snapshot, or go online for latest versions)
3) Run a sketchy Python server on the host that executes commands it gets on loopback from the VM. (Included)
4) Write grammar in a CFG style to describe the commands you want to Dragon.
5) I am currently willing to answer questions about getting this working to anyone who has read the Detail section and is still having problems.

-------------------------------------------------------------------------------------------

Detail
------

1) Install Windows on a virtual machine (I use VirtualBox). I used Windows 7 Ultimate 32 bit. In theory XP is supported by Dragon 12, but I couldn't make it work. You'll want at least 2 GB of RAM (more if you can afford it) and a reasonable hard disk -- I used 35 GB dynamic; it's using about 10. I also recommend two processors, if you can afford them. Make sure you create your Dragon profile (in step 2) on the number of processors you will be using overall.

2) Install Dragon. I used Dragon NaturallySpeaking Premium 12. I believe you need at least Premium to access NatLink. Create your profile according to their directions when you get to need that setting screen you should choose "Best Match IV", in the advanced settings. "Best Match V", the default, poorly with NatLink. In particular, it tends to be slower for the sorts of things will be doing. You will not be able to change this later!

3) install python 2.7.5 (other versions may work but I have included this version because the first few I tried did not)

4) install pywin32. The version you use should match the version of Python you install. Once again I have included a working version.

5) install NatLink. There are several distributions available online however the included one is the only one I was able to get work. It does come with a lot of extras we won't be using such as Unimacro and Vocola.

6) go to NatLink under the start menu, and selected GUI configuration. You will need to enable it, and this will require you to close Dragon.

7) install dragonfly. Make sure you install it to a path with no spaces in it. I have included a dynamically linked library if you get a missing DLL error you can copy into the system 32 folder under Windows.

8) now when you start Dragon, a NatLink window should pop up with the title "Messages form Natlink". This is where messages from your scripts sent to standard output and standard error will appear.

9) create a virtual box shared folder. I will be referring to it as the E drive.

10) Move the checkout of this project into the root of the E Drive (eg, on windows this file is E:\aenea\README.txt). Dragonfly modules will live in the grammar subdirectory of aenea. Note that these modules are executed under Windows not Linux. You may use the modules in you till in your scripts.

11) on the Linux host, install xdotool, which should be available from standard repositories. I would also recommend a tiling window manager (I use Notion), or other environment that is easy to control via keyboard, as keyboard macros are much easier to write than mouse macros.

12) take a look at the grammar modules if you wish to create your own. Sorry they are so terrible I needed a stopgap measure to let me bootstrap a better system. The main point of note is the Comsat module which you use to control the links hosted via a series of RPC commands.

13) on the Linux host, run the server.py command in a Python interpreter. By default it is configured to listen on loopback only and to port 8240.

14) whenever you speak a command, Dragon will create a new client Comsat connect to the server sends the command and disconnect so you should see one connected client and one lost connection per command you speak in the server output window. This is completely normal.

15) note that only commands you define will be available in Linux, the normal Dragon commands will go to Windows instead. As such I suggest leaving Dragon in command mode.

16) I have my modules configured so that they are only active when a notepad window is open on Windows. That way when writing prose I can use Dragon on Windows normally with none of my custom grammar active. Note that Dragon seems to interpret notepad++ as notepad for these purposes.

17) every time you update your modules you will need to turn the microphone off and then back on again for the changes to be visible. Import errors will disable on a module by module basis and will appear in the messages window. In some cases, particularly when modifying one of the modules in util, you will need to exit Dragon and reenter it in order for the changes to appear.

18) from time to time, Dragon will pop up stupid Windows that you will need to close in the Windows virtual machine. I have not yet found a way to disable these but I have not looked very hard. They seem to be getting less common as it gets better at listening to me and I get better at speaking to it.

19) It would be wise to change the VirtualBox network adapter to host only to isolate your VM from the internet, given it's basically a keylogger...and let's be honest here; you're not going to keep that box up to date.

20) Choose Again.
