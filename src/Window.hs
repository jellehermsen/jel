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

module Window where

import Types
import qualified Buffer

data WindowType = File | Directory | Static
  deriving (Show, Eq)

type WindowId = Types.Id

data Window = Window
    { windowId :: WindowId
    , buffer :: Buffer.BufferId
    , scrollPos :: Types.Position
    , cursorPos :: Types.Position
    , readonly :: Bool
    , size :: Size
    , windowType :: WindowType
    , cWindow :: CWindow
    }

newWindow :: WindowId -> Buffer.BufferId -> CWindow -> Size -> Window
newWindow windowId bufferId cWindow size = Window
    { windowId = windowId
    , buffer = bufferId
    , scrollPos = (0, 0)
    , cursorPos = (0, 0)
    , size = size
    , readonly = False
    , windowType = File
    , cWindow = cWindow
    }

getRelativeCursorPos :: Window -> Types.Position
getRelativeCursorPos window = (cursorY - scrollY, cursorX - scrollX)
    where
        scrollX = snd $ scrollPos window
        scrollY = fst $ scrollPos window
        cursorX = snd $ cursorPos window
        cursorY = fst $ cursorPos window
