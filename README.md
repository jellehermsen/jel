Jel
===

Jel is Jelle's editor. It's a VI-like editor, borrowing some features
from Vim, and removing stuff that the I don't need.

NB: This is mostly exploratory programming at the moment and definitely not
useable.

Requirements
------------
Right now, this project builds on Linux. Most of the BSDs or other "posixy"
systems should probably work too.

You will need to install the Haskell Platform on your machine. Find out more at
[https://www.haskell.org/platform/](https://www.haskell.org/platform/).

You will need the dev package of ncursesw5 and you will need C2HS. You can
install these on Debian-like systems with:

```
sudo apt install c2hs libncursesw5-dev
```

If C2HS isn't available in your package repository you can also install it
using cabal:

```
cabal install c2hs
```

Building
--------
To build this you can use Cabal. In your terminal go to the root folder of this
project and run:

```
cabal sandbox init
cabal update
cabal install --only-dependencies
cabal build
```

Design notes
------------
Here are my notes on the preliminary design.

The entry point for the program is **Main.hs**, and particularly the *main*
function inside this file. This *main* function is responsible for starting the
NCurses environment and initializing the State record in which we keep all our
stateful data.

The basic dataflow of *Jel* is relatively simple. **Main.hs** has a *loop*
function that waits until it gets an event from NCurses. When it does,
it tries to parse the given input and transform that into a list of (partial)
commands and a list of actions. When it has succesfully mapped an input to an
action, the state record is altered and using the resulting events the screen
is updated.

Parsing the input is done through the *parseInput* function which you can find
in **Input.hs**.

The *parseInput* function maps the current mode we are in (command mode, or
visual mode..etc), the previous list of buffered commands and the curses event
onto either a new list of commands, or a list of actions. It uses pattern
matching to match the current contents of the given commands list and if it
finds a complete match for an action, it gives you that action (or list of
actions).  If it can't yet map the current list of commands onto an action, it
will simply tack the last command onto the list of commands and give that back,
or it will give you an empty list of commands if the current list of commands
will never map to a possible command.

When the input is parsed, we update the state with the last command, and we
handle all the actions using the *handleActions* function, which recursively
changes the state using a list of actions until it's done or one of the actions
fails. Changing the state is done using the *changeState* function which you
can find in **StateChange.hs**. Besides returning a changed state record, it
also gives you a list of events. These events can be regarded as side effects
for the screen or the disk, that have not been executed yet. This list of
events is folded over the state record using the *handleEvent* function you can
find in **Event.hs**. These events encompass everything from saving the file to
disk to moving the cursor or changing some text.

If you look at the design linearly you see that I've tried to seperate the
editor in a couple of different layers. The first layer are the commands you
enter. These can be gradually build up, like you're used to in VI. When a
command is completely built it can result in an action. There's a large variety
of actions, but eventually they all boil down to only a couple of actual
events. So for example there's an action for moving the cursor a page down, or
to the first none whitespace character, but when translated into an event
there's only one "move the cursor" event.

By making this layered separation I hope that I'll be able to define all the
functionality in a pretty straightforward fashion, without too much
duplication.  GUI events arise naturally from actions, which are translated
easily from commands. If this system works out well it should be relatively
easy to add all the editing goodness I want to have, like macros and multi
window, multi tabbed screens.
