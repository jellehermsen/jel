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
    along with Jel.  If not, see <https://www.gnu.org/licenses/>.
-}

module Input where

import Data.Char (isPrint)
import qualified UI.NCurses as Curses
import Types

parseInput :: Mode -> [Command] -> Curses.Event -> Either [Command] [Action]

-- Numbers
parseInput CommandMode [] (Curses.EventCharacter '1') = Left [CmdAmount 1]
parseInput CommandMode [] (Curses.EventCharacter '2') = Left [CmdAmount 2]
parseInput CommandMode [] (Curses.EventCharacter '3') = Left [CmdAmount 3]
parseInput CommandMode [] (Curses.EventCharacter '4') = Left [CmdAmount 4]
parseInput CommandMode [] (Curses.EventCharacter '5') = Left [CmdAmount 5]
parseInput CommandMode [] (Curses.EventCharacter '6') = Left [CmdAmount 6]
parseInput CommandMode [] (Curses.EventCharacter '7') = Left [CmdAmount 7]
parseInput CommandMode [] (Curses.EventCharacter '8') = Left [CmdAmount 8]
parseInput CommandMode [] (Curses.EventCharacter '9') = Left [CmdAmount 9]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '1') = Left [CmdAmount (n*10+1)]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '2') = Left [CmdAmount (n*10+2)]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '3') = Left [CmdAmount (n*10+3)]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '4') = Left [CmdAmount (n*10+4)]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '5') = Left [CmdAmount (n*10+5)]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '6') = Left [CmdAmount (n*10+6)]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '7') = Left [CmdAmount (n*10+7)]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '8') = Left [CmdAmount (n*10+8)]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '9') = Left [CmdAmount (n*10+9)]
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '0') = Left [CmdAmount (n*10)]

-- Movement
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'j') = Right $ matchActions [CmdAmount n, CmdDown]
parseInput CommandMode [] (Curses.EventCharacter 'j') = Right $ matchActions [CmdDown]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'k') = Right $ matchActions [CmdAmount n, CmdUp]
parseInput CommandMode [] (Curses.EventCharacter 'k') = Right $ matchActions [CmdUp]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'h') = Right $ matchActions [CmdAmount n, CmdLeft]
parseInput CommandMode [] (Curses.EventCharacter 'h') = Right $ matchActions [CmdLeft]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'l') = Right $ matchActions [CmdAmount n, CmdRight]
parseInput CommandMode [] (Curses.EventCharacter 'l') = Right $ matchActions [CmdRight]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '$') = Right $ matchActions [CmdAmount n, CmdEndOfLine]
parseInput CommandMode [] (Curses.EventCharacter '$') = Right $ matchActions [CmdEndOfLine]

parseInput CommandMode [] (Curses.EventCharacter '^') = Right $ matchActions [CmdFirstNoneWhiteSpace]

parseInput CommandMode [] (Curses.EventCharacter '0') = Right $ matchActions [CmdBeginningOfLine]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '\EOT') = Right $ matchActions [CmdAmount n, CmdPageDown]
parseInput CommandMode [] (Curses.EventCharacter '\EOT') = Right $ matchActions [CmdPageDown]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '\NAK') = Right $ matchActions [CmdAmount n, CmdPageUp]
parseInput CommandMode [] (Curses.EventCharacter '\NAK') = Right $ matchActions [CmdPageUp]

-- Delete character(s)
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'x') = Right $ matchActions [CmdAmount n, CmdDeleteChar]
parseInput CommandMode [] (Curses.EventCharacter 'x') = Right $ matchActions [CmdDeleteChar]

-- Open new line
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'o') = Right $ matchActions [CmdAmount n, CmdOpenLine]
parseInput CommandMode [] (Curses.EventCharacter 'o') = Right $ matchActions [CmdOpenLine]

-- Switch to insert mode
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'i') = Right $ matchActions [CmdInsertMode]
parseInput CommandMode [] (Curses.EventCharacter 'i') = Right $ matchActions [CmdInsertMode]

-- Switch back to command mode
parseInput CommandMode _ (Curses.EventCharacter '\ESC') = Left []
parseInput InsertMode [] (Curses.EventCharacter '\ESC') = Right $ matchActions [CmdCommandMode]

-- Insert newline
parseInput InsertMode [] (Curses.EventCharacter '\n') = Right $ matchActions [CmdInsertNewLine]

-- Insert character
parseInput InsertMode [] (Curses.EventCharacter c) = if isPrint c
    then
        Right $ matchActions [CmdInsertChar c]
    else
        Left []

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'u') = Right $ matchActions [CmdAmount n, CmdUndo]
parseInput CommandMode [] (Curses.EventCharacter 'u') = Right $ matchActions [CmdUndo]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '\DC2') = Right $ matchActions [CmdAmount n, CmdRedo]
parseInput CommandMode [] (Curses.EventCharacter '\DC2') = Right $ matchActions [CmdRedo]

-- Quit on ctrl-q
parseInput mode cmds (Curses.EventCharacter '\DC1') = Right [ActQuit]

parseInput _ _ _ = Left []

-- Map a list of commands to a list of actions
matchActions :: [Command] -> [Action]
matchActions [CmdQuit] = [ActQuit]
matchActions [CmdAmount n, CmdDown] = [ActCursorDown n]
matchActions [CmdDown] = [ActCursorDown 1]

matchActions [CmdAmount n, CmdUp] = [ActCursorUp n]
matchActions [CmdUp] = [ActCursorUp 1]

matchActions [CmdAmount n, CmdLeft] = [ActCursorLeft n]
matchActions [CmdLeft] = [ActCursorLeft 1]

matchActions [CmdAmount n, CmdRight] = [ActCursorRight n]
matchActions [CmdRight] = [ActCursorRight 1]

matchActions [CmdAmount n, CmdEndOfLine] = [ActCursorDown (n - 1), ActEndOfLine]
matchActions [CmdEndOfLine] = [ActEndOfLine]

matchActions [CmdBeginningOfLine] = [ActBeginningOfLine]
matchActions [CmdFirstNoneWhiteSpace] = [ActFirstNoneWhiteSpace]

matchActions [CmdAmount n, CmdPageDown] = [ActPageDown n]
matchActions [CmdPageDown] = [ActPageDown 1]

matchActions [CmdAmount n, CmdPageUp] = [ActPageUp n]
matchActions [CmdPageUp] = [ActPageUp 1]
matchActions [CmdInsertMode] = [ActInsertMode]
matchActions [CmdCommandMode] = [ActCommandMode]
matchActions [CmdInsertChar c] = [ActInsertChar c]
matchActions [CmdAmount n, CmdDeleteChar] = [ActDeleteChar n]
matchActions [CmdDeleteChar] = [ActDeleteChar 1]
matchActions [CmdInsertNewLine] = [ActInsertNewLine, ActCursorDown 1, ActFirstNoneWhiteSpace]
matchActions [CmdAmount n, CmdOpenLine] = matchActions [CmdOpenLine]
matchActions [CmdOpenLine] = [ActCursorDown 1, ActBeginningOfLine, ActInsertNewLine, ActInsertMode]

matchActions [CmdAmount n, CmdUndo] = [ActUndo n]
matchActions [CmdUndo] = [ActUndo 1]

matchActions [CmdAmount n, CmdRedo] = [ActRedo n]
matchActions [CmdRedo] = [ActRedo 1]
matchActions _ = [ActIdle]
