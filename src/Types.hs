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

type V2 = (Int, Int)
type V4 = (Int, Int, Int, Int)
type Id = Int
type Position = V2
type CWindow = Curses.Window
type Size = V2

data Mode = CommandMode | InsertMode | LastLineMode | VisualMode |
            VisualLineMode | ReplaceMode
  deriving (Show, Eq)

data Command = CmdRight 
    | CmdLeft
    | CmdUp
    | CmdDown
    | CmdPageDown
    | CmdPageUp
    | CmdAmount Int
    | CmdPaste
    | CmdYank
    | CmdDeleteLine
    | CmdDeleteChar
    | CmdQuit
    | CmdEndOfLine
    | CmdBeginningOfLine
    | CmdFirstNoneWhiteSpace
    | CmdInsertMode
    | CmdCommandMode
    | CmdInsertChar Char
    | CmdInsertNewLine

-- All the possible actions in the editor
data Action = ActIdle
    | ActBeginningOfLine
    | ActCommandMode
    | ActCursorDown Int
    | ActCursorLeft Int
    | ActCursorRight Int
    | ActCursorUp Int
    | ActDeleteChar Int
    | ActEndOfLine
    | ActFirstNoneWhiteSpace
    | ActInsertChar Char
    | ActInsertMode
    | ActInsertNewLine
    | ActPageDown Int
    | ActPageUp Int
    | ActQuit

-- All the events, which resemble disk operations or GUI changes
data Event = EvIdle
    | EvCursorTo Int Int
    | EvOpenFile
    | EvSaveFile
    | EvQuit
    | EvInsertMode
    | EvCommandMode
    | EvInsertChar Char
    | EvDeleteChar Int
