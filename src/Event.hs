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

module Event where

import qualified UI.NCurses as Curses

import Types
import qualified Gui
import qualified State
import qualified Window

handleEvent :: State.State -> Event -> Curses.Curses (State.State)

handleEvent state EvQuit = error "Quit"
handleEvent state _ = return state
