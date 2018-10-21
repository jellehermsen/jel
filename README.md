Jel
===

Jel is Jelle Hermsen's editor. It's a VI-like editor, borrowing some features
from Vim, and removing stuff that the author doesn't need.

NB: This is still work in progress and not useable at the moment.

Requirements
------------
Right now, this project builds on Linux. Most of the BSDs should probably work
too. You will need to dev package of ncursesw5 and you will need C2HS. You can
install these on Debian-like systems with:

`
sudo apt install c2hs libncursesw5-dev
`

You will also need to install the Haskell Platform on your machine. Find out more at (https://www.haskell.org/platform/)[https://www.haskell.org/platform/].

Building
--------
To build this you can use Cabal. In your terminal go to the root folder of this
project and run:

`
cabal sandbox init

cabal update

cabal install --only-dependencies

cabal build
`

