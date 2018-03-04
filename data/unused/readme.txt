Readme.txt
Last changed: March 21, 1999

StoryHarp Audioventure Authoring System version 1.32
Copyright 1998, 99 Paul D. Fernhout & Cynthia F. Kurtz
All Rights Reserved
See the file license.txt for details on the StoryHarp license.

With StoryHarp you can create and play adventures using speech recognition and text-to-speech, sound, and music. 

You can use StoryHarp in "Player-only mode" to play StoryHarp world files indefinitely without registering. If you use the StoryHarp editor for more than 24 hours of total use, you are legally required to register it. Registration is also required if you want to distribute anything you produce using StoryHarp in other than Player-only mode. See the license (in license.txt) for details. 

The "StoryHarp Player" icon starts StoryHarp in Player-only mode. The "StoryHarp" icon starts StoryHarp in normal (evaluation or registered) mode.

=== Version 1.32 Notes (March 21, 1999)

These are the changes from version 1.31 to version 1.32 of StoryHarp:
* Removed check-for-newer-version reminder if in player-only mode.
* Fixed potential bug with international Windows versions when loading ini file.
* Tested with ViaVoice 98 and Microsoft Agent. No problems found. Previous versions of ViaVoice may have problems with Agent.
* Tested with Agent 2.0. No problems found.

=== Version 1.31 Notes (November 30, 1998)

These are the changes from version 1.3 to version 1.31 of StoryHarp:
* Removed pricing information from program and help system. At the time of this release registration of StoryHarp is US$19.95 per copy; this price is subject to change without notice. For orders over ten copies contact us directly for discounts. See our web site for the most current pricing information.

=== Version 1.3 Notes (June 1, 1998)

These are the changes from version 1.2 to version 1.3 of StoryHarp:
* Added Java 1.0 applet source code generation for putting CYOA stories on web sites. (The Java applet created only supports reading a text transcript and picking a choice from a list.)
* Added right-click popup menu in map to make contexts, commands, and moves.
* Added support for mixed case in contexts, moves, commands, requirements, and changes. 
* Added limited graphics support with picture window and picture macro for "accent" BMP pictures.
* Added popup hints for choices, table entries, and browser list items.
* Added ability to change font in player window.
* Additional improvements to autoscrolling to fix problem where not all text was scrolled.
* Better centering for map when chosen to handle when no map items are selected.
* Removed '$' in transcript window when riddles are selected.
* Added name of context in New Command Wizard page for adding commands. 
* Prevented problem when shutting down with Agent running by warning that StoryHarp needs to be closed first. 
* Can now drag and drop session or world files onto StoryHarp player or editor windows.
* Changed caption for main window to have session name at the end in parentheses.
* Updated help system to reflect changes.

Special notes on case conversions and file versions:
StoryHarp now writes world files with a version 1.3 header instead of a version 1.0 header.
The files GarTrek.wld, Intro.wld and Astronomy Test.wld have been changed to use mixed case for proper names and saved as version 1.3 files. 
Versions of StoryHarp before 1.3 can read version 1.3 files, but they will convert all fields except replies in these files to all lower case if you look at the rule containing them in the world editor. 
Our advice is to start using StoryHarp 1.3 to avoid this potential problem.

Many thanks to our users for their feedback and suggestions!

=== Version 1.2 Notes (May 14, 1998)

These are the changes from version 1.1 to version 1.2 of StoryHarp:
* Changed the font from MS Sans Serif to Arial to accomodate large fonts. This means that you need to have the Arial font installed to use StoryHarp. The Arial font comes standard with Windows and will be there unless you have removed it.
* Improved refreshing of command options after you edit rules and load files.
* Fixed a scrolling text problem in the player window.
* Modified the /P command-line option to no longer record use time or remind you to register.
* Changed menu items from "Session | Start in World" to "Session | Open World" and "Session | Open" to "Session | Open Saved Session".
* Reduced the single-user registration price from $79.95 to $39.95.
* Changed the license so that use of StoryHarp in player-only mode does not require registration.
* Added Start menu shortcut to access StoryHarp in player-only mode.
* Fixed problem linking license button to license page in the help system.
* Made minor changes to the help system.

=== Version 1.1 Notes (May 1, 1998)

These are the changes from version 1.0 to version 1.1 of StoryHarp:
* Window positions on startup for a completely new installation now fit on 640X480 screens. You can (as always) resize windows to fit your screen.
* Added "World | Switch to Player" menu item to world editor window.
* Renamed "Session | New for World" menu item to "Session | Start in World".
* Changed web link referenced in register window text field from 
"http://www.kurtz-fernhout.com/order" to "http://www.kurtz-fernhout.com/order.htm".
* Changed error dialog which appears at startup if Microsoft Agent is not installed. The dialog can now 
be told not to reappear by checking a checkbox.
* Fixed a memory access exception that could occur when using "World | New" if you had previously edited an existing world and not saved it first.
* Updated the help system to reflect these changes.
* Explained in the help system how to switch between windows and resize windows.
* Change accelerator keys on some menu items.
* Made minor modifications to the GarTrek.wld story.
* Modified the positioning algorithm which places new variables and rules in the map when they are made using the World editor window. The algorithm now uses random positioning around the last object selected (or middle of the map if none is selected). Previously, new items were placed either directly over old ones at the center of the map or a fixed distance below the last selected item. Wizards still use their old positioning algorithm which attempts to place the items they create as structures going down the page.

=== Version 1.0 Notes (April 3, 1998)

StoryHarp version 1.0 was released on April 3, 1998.

=== Notes on Microsoft Agent
You must install Microsoft Agent and a TTS and SR engine if you want to use the SR, TTS, or sound features. Otherwise, adventures are playable by clicking on options and reading the transcript. See the help system for details. If you haven't installed Agent, you will get a popup warning message when you start the program. This dialog has a check box you can select if you don't want to see it again.

When an Agent speaks, it might break up or stutter on slower computers, such as a 100 MHz Pentium. If an Agent breaks up or stutters when saying text, try using the SmallHarp or Smiley Agent character instead, or download Microsoft's Genie character from their web site (see our web site for links). Those characters seem to work better because of their smaller size and choice of voice.

=== System requirements

You need Windows 95 or Windows NT 4.0 to run StoryHarp. If you plan to use StoryHarp with Microsoft Agent, you will need at least a 100 MHz Pentium. (Actually, we've seen it run OK on a 486 DX2 66 using the SmallHarp Agent character, so you could try it on a lesser machine.) If you intend to run StoryHarp without Microsoft Agent, probably any 486 that can run Windows 95 will work. You will need about 5 MB of hard disk space for StoryHarp. You should have at least 4 MB of free memory on your computer to run StoryHarp. 

If you install Microsoft Agent and its associated TTS and SR engines, you will need at least 20 MB of hard disk space, and significant free memory. Microsoft Agent may also require that Microsoft Internet Explorer 3.02 or later be installed in order to install Agent. See the StoryHarp help system for details.

=== Packing list

Program files:
StoryHarp.exe - the program
StoryHarp.hlp - the help system
StoryHarp.cnt - the table of contents for the help system
Readme.txt    - this file
License.txt   - the software license
uninst0.000   - file needed for uninstaller
uninst1.000   - file needed for uninstaller

Microsoft Agent characters:
StoryHarp.acs - Harp character
SmallHarp.acc - Small harp character (runs better on slower machines)
Swan.acs      - Swan character
Smiley.acs    - Smiley Face character

MIDI music files used by examples:
short.mid  
sunflower.mid

WAV sound files used by examples:
Crunch1.wav
Flute1.wav
Flute2.wav
Flute3.wav
Squeak1.wav
Squeak2.wav
Water1.wav

Java-related files:
Story.htm - Example web page that references a java applet that plays StoryHarp stories
Story.class - Example Java applet compiled by JDK 1.0
Story.java - Source for example Java applet based on Astronomy Test.wld
Template.java - Standard source code used in for the beginning the Java applet source file

Example picture files:
spacegarden.bmp - Example bitmap from NASA used for a demonstration of the picture macro in GarTrek.wld

Example world files:
Astronomy Test.wld - multiple choice questions about astronomy and space
GarTrek.wld - the longest example audioventure (includes modified rules from the files Intro.wld, Advanced Tutorial.wld, and Grue Pit.wld)
Grue Pit.wld - example of simple sequence
House and Yard.wld - example set around a house
Intro.wld - example referenced by help system in "A simple example audioventure"
Max the Computer.wld - example of a conversation
Tutorial Advanced.wld - what you would have after completing the advanced tutorial
Tutorial Basic.wld - what you would have after completing the basic tutorial
Tutorial Intermediate.wld - what you would have after completing the intermediate tutorial

=== Notes on uninstalling
The StoryHarp installation program installs no DLLs or other modules. You can uninstall StoryHarp using the Add/Remove Programs item in the Control Panel or by running the Uninstall program in the StoryHarp group. Before you uninstall StoryHarp you should back up any files in the StoryHarp directory you made changes to that you want to keep. Any files you add to the StoryHarp directory will not be removed by the uninstaller. A StoryHarp.ini file with registration information will remain in the Windows directory. If you delete this file you will lose your registration information. The StoryHarp installation program adds information to the Uninstall folder in the Registry, but StoryHarp itself does not modify the Windows Registry when it runs. 

You can also just delete the StoryHarp directory and any shortcuts yourself; however, this will leave some information in the Uninstall folder in the Registry (search for "StoryHarp") which you have to delete yourself.

The StoryHarp.ini file in your Windows directory is not modified when you uninstall or reinstall. If you registered an earlier version, your registration information will still be valid and you should not have to reenter it.

If you install Microsoft Agent or the Microsoft supplied SR or TTS engines, and wish to remove them, follow the directions that came with them for uninstalling. Most likely, this will be done from the Add/Remove Programs item in the Control Panel. 

=== Notes on installing over a previous version of StoryHarp
If you are installing StoryHarp over a previous version, we recommend you uninstall the previous StoryHarp version, then install the new version. Before you uninstall StoryHarp you should back up any files in the StoryHarp directory you made changes to that you want to keep.

If you do install over an existing StoryHarp version, you may find two entries for StoryHarp in your Add/Remove Programs control panel. As long as you don't delete the uninstall files (Uninst0.000, Uninst1.000, Uninst0.001, Uninst1.001, etc) you should be able to remove both installations.

=== Notes on extra sounds and music
Some music files the examples reference may be only available in the Windows NT media directory. Many sound files GarTrek.wld references are only available on the widely distributed 1993 CD Sound Library Pro CD by Wayzata Technology. To hear these sounds, you need to put that CD in your CD-ROM player, and set the extra sounds and music directory to the drive letter of the CD-ROM player. For improved search perfomance, you can set the extra sounds and music directory to the D:\Sounds\Wav directory on that CD-ROM (where D: is your CD-ROM drive letter). That CD was sometimes bundled with hardware, so chances are you or a friend may have it around somewhere.

=== Other notes
StoryHarp is a trademark of Paul D. Fernhout and Cynthia F. Kurtz

For the most up-to-date information on StoryHarp, see our web site at:
http://www.kurtz-fernhout.com

=== GETTING STARTED
To run the program, click the Start button, then choose Programs, then choose the StoryHarp group, then choose the StoryHarp program. When you start StoryHarp for the first time, choose "Session | Open World" and pick a world file to start playing it. 