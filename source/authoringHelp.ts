import { Glyph } from "./VariablesView"

// Taken from website

export const authoringHelp = `
The StoryHarp authoring environment was designed to be as simple as possible
to help you, the author, focus on creative writing.
Simple as it is, there are still several basic concepts you need to know to get started.
They form the core building blocks with which you can build adventures of a certain style.

The key ideas and their icons in the interface are: command (${Glyph.command}), reply (${Glyph.reply}), move (${Glyph.move}),
requirements (${Glyph.requirements}), variables (${Glyph.present}), changes (${Glyph.changes}), context (${Glyph.context}), focus, and rules.

A StoryHarp adventure consists of commands (${Glyph.command}) chosen by the user which produce replies (${Glyph.reply}) announced by the system.
For example, commanding "examine the vase" might produce a reply of
"The vase is fairly heavy, with a blue green glaze. The word 'sassafras' is etched on the bottom."
Replies often give you clues to other things you can do later in the adventure.
In this case, you might need to use the word 'sassafras' somehow.
In order to preserve a sense of interactivity, most replies should be no longer than a few sentences.

Of course, not all commands may be available at once.
Commands may have requirements (${Glyph.requirements}) which must be met before they are available.

Requirements are specified in terms of global variables that may be true (${Glyph.present}) or false (${Glyph.absent}).
Requirements are specified as a list of variables, and whether each variable needs to be true or false.
Each variable has a name, which is usually a short phrase, like 'wearing glasses' or 'barred from the hotel'.

Commands may make changes (${Glyph.changes}) to the state of variables, which in turn may affect the availability of commands.
Changes are specified as a list of variables, and whether each variable will be set to true or false.

Most commands only make sense in a certain context (${Glyph.context}), which is usually defined by a physical location.
For example, you might be able to examine the vase only if you are standing near the fireplace.

To support this most common situation, every command is assigned a context.
A context is associated with a variable of the same name which records whether the context is active (true) or inactive (false).
A command's context must be active for the command to be available.
A context could be thought of as another requirement for a command.
Contexts were set off from other requirements to provide a way to organize commands.

The most common action in StoryHarp is usually traveling from one location (context) to another.
So, usually, you only want one context active at a time,
and therefore want one to become inactive (false) when another one becomes active (true).

To support this, StoryHarp maintains a focus on one variable at a time (the focused context).
So, the most common change is to move (${Glyph.move}) the focus from one context to another.
This sets the old focus variable to false and sets the new focus variable to true.
More than one context can be active at a time, but only one active context can have the focus.

Each entered combination of context, other required variables, command phrase,
reply, movement of focus, and other changes to variables, is called a rule.
The context and other requirements determine if the command in the rule is available for the user to choose.

If a command is available and the player chooses command phrase,
then the reply is announced, a move is done (if defined),
and then any other changes to variables are made according to the rule.

All available rules which have a command that matches what the player chose
are done in the order in which they are in the list of rules.

For example, you might have two 'look' commands for one context,
the second of which is only active if the player is wearing glasses.
When the player says 'look', they may get one or two replies,
depending on whether or not the player is wearing glasses.

You can also use the same command phrase in different contexts
to do different things (like 'go north' in different rooms takes you to different places).

For a musical metaphor, you can think of rules as strings in a harp.
The commands are the fingers you use to pluck the strings,
and the replies are the sounds the strings make.
The other fields of the rules set up the musical style that determines what sequences of notes make sense.
The transcript is like the song the player creates with the harp in the style of the story.
`
