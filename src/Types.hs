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
import qualified Data.Map.Strict as Map
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

type Registers = Map.Map Text.Text Text.Text

data Command = CmdAmount Int
    | CmdAppend
    | CmdBeginningOfLine
    | CmdCommandMode
    | CmdDelete Int
    | CmdDeleteChar
    | CmdDeleteCharBefore
    | CmdDeleteLine Int
    | CmdDown
    | CmdEndOfLine
    | CmdFirstNoneWhiteSpace
    | CmdNextWord
    | CmdPrevWord
    | CmdNextWordEnding
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
    | CmdJoinLine
    | CmdFindForward
    | CmdFindBackward
    | CmdChar Char
    | CmdGotoLastLine
    | CmdGotoLine
    | CmdGotoFirstLine
    | CmdReplaceChar Int
    deriving (Show)

data Action = ActIdle
    | ActBeginningOfLine
    | ActCommandMode
    | ActCursorDown Int
    | ActCursorLeft Int
    | ActCursorRight Int
    | ActCursorUp Int
    | ActDeleteChar Int
    | ActDeleteCharBefore Int
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
    | ActJoinLine Int
    | ActFindForward Int Char
    | ActFindBackward Int Char
    | ActGotoLine Int
    | ActGotoLastLine
    | ActReplaceChar Int Char
    | ActNextWord Int
    | ActPrevWord Int
    | ActNextWordEnding Int
    deriving (Show)

-- |All the events, which resemble disk operations or GUI changes
data Event = EvIdle
    | EvOpenFile
    | EvSaveFile
    | EvQuit
    | EvRedrawScreen
