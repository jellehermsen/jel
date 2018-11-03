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

import qualified UI.NCurses as Curses
import Types

parseInput :: Mode -> [Command] -> Curses.Event -> ([Command], [Action])

-- Numbers
parseInput CommandMode [] (Curses.EventCharacter '1') = ([CmdAmount 1], [ActIdle])
parseInput CommandMode [] (Curses.EventCharacter '2') = ([CmdAmount 2], [ActIdle])
parseInput CommandMode [] (Curses.EventCharacter '3') = ([CmdAmount 3], [ActIdle])
parseInput CommandMode [] (Curses.EventCharacter '4') = ([CmdAmount 4], [ActIdle])
parseInput CommandMode [] (Curses.EventCharacter '5') = ([CmdAmount 5], [ActIdle])
parseInput CommandMode [] (Curses.EventCharacter '6') = ([CmdAmount 6], [ActIdle])
parseInput CommandMode [] (Curses.EventCharacter '7') = ([CmdAmount 7], [ActIdle])
parseInput CommandMode [] (Curses.EventCharacter '8') = ([CmdAmount 8], [ActIdle])
parseInput CommandMode [] (Curses.EventCharacter '9') = ([CmdAmount 9], [ActIdle])
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '1') = ([CmdAmount (n*10+1)], [ActIdle])
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '2') = ([CmdAmount (n*10+2)], [ActIdle])
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '3') = ([CmdAmount (n*10+3)], [ActIdle])
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '4') = ([CmdAmount (n*10+4)], [ActIdle])
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '5') = ([CmdAmount (n*10+5)], [ActIdle])
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '6') = ([CmdAmount (n*10+6)], [ActIdle])
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '7') = ([CmdAmount (n*10+7)], [ActIdle])
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '8') = ([CmdAmount (n*10+8)], [ActIdle])
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '9') = ([CmdAmount (n*10+9)], [ActIdle])
parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '0') = ([CmdAmount (n*10)], [ActIdle])

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'j') = ([], [matchAction [CmdAmount n, CmdDown]])
parseInput CommandMode [] (Curses.EventCharacter 'j') = ([], [matchAction [CmdDown]])

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'k') = ([], [matchAction [CmdAmount n, CmdUp]])
parseInput CommandMode [] (Curses.EventCharacter 'k') = ([], [matchAction [CmdUp]])

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'h') = ([], [matchAction [CmdAmount n, CmdLeft]])
parseInput CommandMode [] (Curses.EventCharacter 'h') = ([], [matchAction [CmdLeft]])

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter 'l') = ([], [matchAction [CmdAmount n, CmdRight]])
parseInput CommandMode [] (Curses.EventCharacter 'l') = ([], [matchAction [CmdRight]])

parseInput CommandMode [CmdAmount n] (Curses.EventCharacter '$') = ([], [matchAction [CmdAmount n, CmdDown], matchAction [CmdEndOfLine]])
parseInput CommandMode [] (Curses.EventCharacter '$') = ([], [matchAction [CmdEndOfLine]])

parseInput CommandMode [] (Curses.EventCharacter '0') = ([], [matchAction [CmdBeginningOfLine]])

parseInput CommandMode [] (Curses.EventCharacter '\EOT') = ([], [matchAction [CmdPageDown]])
parseInput CommandMode [] (Curses.EventCharacter '\NAK') = ([], [matchAction [CmdPageUp]])
parseInput mode cmds ev = error $ show ev

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

matchAction [CmdPageDown] = ActPageDown 1
matchAction [CmdPageUp] = ActPageUp 1
matchAction _ = ActIdle
