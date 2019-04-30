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

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'j') = Right [matchAction [CmdAmount n, CmdDown]]
parseInput CommandMode [] (Curses.EventCharacter 'j') = Right [matchAction [CmdDown]]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'k') = Right [matchAction [CmdAmount n, CmdUp]]
parseInput CommandMode [] (Curses.EventCharacter 'k') = Right [matchAction [CmdUp]]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'h') = Right [matchAction [CmdAmount n, CmdLeft]]
parseInput CommandMode [] (Curses.EventCharacter 'h') = Right [matchAction [CmdLeft]]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'l') = Right [matchAction [CmdAmount n, CmdRight]]
parseInput CommandMode [] (Curses.EventCharacter 'l') = Right [matchAction [CmdRight]]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '$') = Right [matchAction [CmdAmount n, CmdDown], matchAction [CmdEndOfLine]]
parseInput CommandMode [] (Curses.EventCharacter '$') = Right [matchAction [CmdEndOfLine]]

parseInput CommandMode [] (Curses.EventCharacter '^') = Right [matchAction [CmdFirstNoneWhiteSpace]]

parseInput CommandMode [] (Curses.EventCharacter '0') = Right [matchAction [CmdBeginningOfLine]]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '\EOT') = Right [matchAction [CmdAmount n, CmdPageDown]]
parseInput CommandMode [] (Curses.EventCharacter '\EOT') = Right [matchAction [CmdPageDown]]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '\NAK') = Right [matchAction [CmdAmount n, CmdPageUp]]
parseInput CommandMode [] (Curses.EventCharacter '\NAK') = Right [matchAction [CmdPageUp]]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'x') = Right [matchAction [CmdAmount n, CmdDeleteChar]]
parseInput CommandMode [] (Curses.EventCharacter 'x') = Right [matchAction [CmdDeleteChar]]

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'i') = Right [matchAction [CmdInsertMode]]
parseInput CommandMode [] (Curses.EventCharacter 'i') = Right [matchAction [CmdInsertMode]]
parseInput CommandMode _ (Curses.EventCharacter '\ESC') = Left []
parseInput InsertMode [] (Curses.EventCharacter '\ESC') = Right [matchAction [CmdCommandMode]]

parseInput InsertMode [] (Curses.EventCharacter '\n') = Right [matchAction [CmdInsertNewLine]]

parseInput InsertMode [] (Curses.EventCharacter c) = if isPrint c
    then
        Right [matchAction [CmdInsertChar c]]
    else
        Left []

parseInput mode cmds ev = error $ show ev
-- parseInput _ _ _ = Left []

matchAction :: [Command] -> Action
matchAction [CmdQuit] = ActQuit
matchAction [CmdAmount n, CmdDown] = ActCursorDown n
matchAction [CmdDown] = ActCursorDown 1

matchAction [CmdAmount n, CmdUp] = ActCursorUp n
matchAction [CmdUp] = ActCursorUp 1

matchAction [CmdAmount n, CmdLeft] = ActCursorLeft n
matchAction [CmdLeft] = ActCursorLeft 1

matchAction [CmdAmount n, CmdRight] = ActCursorRight n
matchAction [CmdRight] = ActCursorRight 1
matchAction [CmdEndOfLine] = ActEndOfLine
matchAction [CmdBeginningOfLine] = ActBeginningOfLine
matchAction [CmdFirstNoneWhiteSpace] = ActFirstNoneWhiteSpace

matchAction [CmdAmount n, CmdPageDown] = ActPageDown n
matchAction [CmdPageDown] = ActPageDown 1

matchAction [CmdAmount n, CmdPageUp] = ActPageUp n
matchAction [CmdPageUp] = ActPageUp 1
matchAction [CmdInsertMode] = ActInsertMode
matchAction [CmdCommandMode] = ActCommandMode
matchAction [CmdInsertChar c] = ActInsertChar c
matchAction [CmdAmount n, CmdDeleteChar] = ActDeleteChar n
matchAction [CmdInsertNewLine] = ActInsertNewLine
matchAction _ = ActIdle
