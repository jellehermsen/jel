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
import qualified Data.Text as Text

type V2 = (Int, Int)
type V4 = (Int, Int, Int, Int)
type Id = Int
type Position = V2
type CWindow = Curses.Window
type Size = V2

data Mode = CommandMode | InsertMode | LastLineMode | VisualMode |
            VisualLineMode | ReplaceMode
  deriving (Show, Eq)

data Command = CmdAmount Int
    | CmdAppend
    | CmdBeginningOfLine
    | CmdCommandMode
    | CmdDelete Int
    | CmdDeleteChar
    | CmdDeleteLine Int
    | CmdDown
    | CmdEndOfLine
    | CmdFirstNoneWhiteSpace
    | CmdInsertChar Char
    | CmdInsertMode
    | CmdInsertModeBefore
    | CmdInsertNewLine
    | CmdLeft
    | CmdOpenLine
    | CmdOpenLineBefore
    | CmdPageDown
    | CmdPageUp
    | CmdPaste
    | CmdQuit
    | CmdRedo
    | CmdRedrawScreen
    | CmdRepeat
    | CmdRight
    | CmdUndo
    | CmdUp
    | CmdYank

-- All the possible actions in the editor
data Action = ActIdle
    | ActBeginningOfLine
    | ActCommandMode
    | ActCursorDown Int
    | ActCursorLeft Int
    | ActCursorRight Int
    | ActCursorUp Int
    | ActDeleteChar Int
    | ActDelete Int [Action]
    | ActDeleteLine Int
    | ActEndOfLine
    | ActFirstNoneWhiteSpace
    | ActInsertChar Char
    | ActInsertMode
    | ActAdvanceCursor
    | ActInsertNewLine
    | ActPageDown Int
    | ActPageUp Int
    | ActQuit
    | ActUndo Int
    | ActRedo Int
    | ActRedrawScreen
    | ActErrorMessage Text.Text
    | ActFlagUndoPoint
    | ActRepeat Int

-- All the events, which resemble disk operations or GUI changes
data Event = EvIdle
    | EvOpenFile
    | EvSaveFile
    | EvQuit
    | EvRedrawScreen
