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
parseInput mode cmds ev = (cmds, [matchAction [CmdQuit]]) 

matchAction :: [Command] -> Action
matchAction [CmdQuit] = ActQuit
matchAction _ = ActIdle
