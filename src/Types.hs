{-
    This file is part of Jel.

    Jel is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    Jel is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Jel. If not, see <https://www.gnu.org/licenses/>.
-}

module Types where

import qualified UI.NCurses as Curses

type Id = Int
type Position = (Integer, Integer)
type CWindow = Curses.Window
type ScreenSize = (Integer, Integer)

data Mode = CommandMode | InsertMode | LastLineMode | VisualMode |
            VisualLineMode | ReplaceMode
  deriving (Show, Eq)

data Command = CmdRight 
    | CmdLeft
    | CmdUp
    | CmdDown
    | CmdPageDown
    | CmdPageUp
    | CmdAmount Integer
    | CmdPaste
    | CmdYank
    | CmdDeleteLine
    | CmdDeleteChar
    | CmdQuit

-- All the possible actions in the editor
data Action = ActIdle
    -- Command mode actions
    | ActCursorLeft Integer
    | ActCursorRight Integer
    | ActCursorUp Integer
    | ActCursorDown Integer
    | ActPageUp Integer
    | ActPageDown Integer
    | ActQuit

    -- Insert mode actions

-- All the GUI actions
data Event = EvIdle
    | EvCursorTo Integer Integer
    | EvOpenFile
    | EvSaveFile
    | EvQuit
