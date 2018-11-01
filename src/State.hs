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

module State where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Types
import Helpers
import qualified Window
import qualified Buffer

type Windows = Map.Map Window.WindowId Window.Window
type Buffers = Map.Map Buffer.BufferId Buffer.Buffer
type Registers = Map.Map Text.Text Text.Text

data State = State
    { buffers :: Buffers
    , windows :: Windows
    , tabs :: [[Window.WindowId]]
    , registers :: Registers
    , activeTab :: Int
    , activeWindow :: Int
    , mode :: Mode
    , lastId :: Id
    , search :: Text.Text
    , command :: [Command]
    , lastAction :: Action
    , lastLine :: Text.Text
    , lastLineCWindow :: CWindow
    , lastLineHistory :: [Text.Text]
    , searchHistory :: [Text.Text]
    , screenSize :: Size
    }

newState :: Window.Window -> CWindow -> (Integer, Integer) -> State
newState firstWindow lastLineCWindow screenSize = State
    { buffers = Map.insert 0 (Buffer.newBuffer 0) Map.empty
    , windows = Map.insert 1 firstWindow Map.empty
    , tabs = [[1]]
    , registers = Map.empty
    , activeTab = 0
    , activeWindow = 1 
    , mode = CommandMode
    , lastId = 1
    , search = ""
    , command = []
    , lastAction = ActIdle
    , lastLine = "Woep woep"
    , lastLineCWindow = lastLineCWindow
    , lastLineHistory = []
    , searchHistory = []
    , screenSize = screenSize
    }

getActiveWindow :: State -> Maybe Window.Window
getActiveWindow state = Map.lookup (activeWindow state) (windows state)

getActiveCWindow :: State -> Maybe CWindow
getActiveCWindow state = case (getActiveWindow state) of
    Nothing  -> Nothing
    Just window -> Just $ Window.cWindow window

getBufferById :: State -> Buffer.BufferId -> Maybe Buffer.Buffer
getBufferById state bufferId = Map.lookup bufferId (buffers state)

getWindowBuffer :: State -> Window.Window -> Maybe Buffer.Buffer
getWindowBuffer state window = getBufferById state (Window.buffer window)

getActiveWindowAndBuffer :: State -> Maybe (Window.Window, Buffer.Buffer)
getActiveWindowAndBuffer state = case getActiveWindow state of
    Nothing -> Nothing
    Just window -> case getBufferById state (Window.buffer window) of
        Nothing -> Nothing
        Just buffer -> Just (window, buffer)

insertBuffer :: State -> Buffer.BufferId -> Buffer.Buffer -> State
insertBuffer state bufferId buffer = state {
    buffers = Map.insert bufferId buffer (buffers state)
}

changeState :: State -> Action -> (Maybe State, [Event])

-- Move cursor down
changeState state (ActCursorDown n) = moveCursor state (n, 0)

-- Move cursor up
changeState state (ActCursorUp n) = moveCursor state (-n, 0)

-- Move cursor left
changeState state (ActCursorLeft n) = moveCursor state (0, -n)

-- Move cursor right
changeState state (ActCursorRight n) = moveCursor state (0, n)

-- Page down
changeState state (ActPageDown n) = moveCursor state (10, 0)

-- Page up
changeState state (ActPageUp n) = moveCursor state (-10, 0)

changeState state ActIdle = (Nothing, [EvIdle])
changeState state _ = (Just state, [EvQuit])

-- Set the scroll position in a window
setScrollPos :: Window.Window -> Window.Window
setScrollPos w = if inWindow 
    then 
        w 
    else 
        w {Window.scrollPos = newScrollPos}
    where
        viewPort = Window.viewPort w
        inWindow = posInRange (Window.cursorPos w) viewPort
        diff = posDiff (Window.cursorPos w) viewPort
        newScrollPos = addV2 (Window.scrollPos w) diff


-- Take scrolling into account
moveCursor :: State -> Position -> (Maybe State, [Event])
moveCursor state dPos = case (getActiveWindowAndBuffer state) of
    Nothing -> (Nothing, [])
    Just (window, buffer) -> 
        let 
            (y, x) = Buffer.closestPos buffer (addPos dPos (Window.cursorPos window))
            newWindow = window { Window.cursorPos = (y, x)} 
        in
            (Just state {
                windows = Map.insert (Window.windowId window) (setScrollPos newWindow) (State.windows state)}
            , [EvCursorTo y x]) 

