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
type ChangedState = Maybe (State, [Event])

data State = State
    { buffers :: Buffers
    , windows :: Windows
    , tabs :: [[Window.WindowId]]
    , registers :: Registers
    , activeTab :: Int
    , activeWindow :: Int
    , command :: [Command]
    , mode :: Mode
    , lastId :: Id
    , search :: Text.Text
    , lastLine :: Text.Text
    , lastLineCWindow :: CWindow
    , lastLineHistory :: [Text.Text]
    , searchHistory :: [Text.Text]
    , macroRegister :: Text.Text
    , dotInput :: Text.Text
    , screenSize :: Size
    }

mkState :: Window.Window -> CWindow -> V2 -> State
mkState firstWindow lastLineCWindow' screenSize' = State
    { buffers = Map.insert 0 (Buffer.mkBuffer 0) Map.empty
    , windows = Map.insert 1 firstWindow Map.empty
    , tabs = [[1]]
    , registers = Map.empty
    , activeTab = 0
    , activeWindow = 1 
    , mode = CommandMode
    , command = []
    , lastId = 1
    , search = ""
    , lastLine = ""
    , lastLineCWindow = lastLineCWindow'
    , lastLineHistory = []
    , searchHistory = []
    , macroRegister = ""
    , dotInput = ""
    , screenSize = screenSize'
    }

getActiveWindow :: State -> Maybe Window.Window
getActiveWindow state = Map.lookup (activeWindow state) (windows state)

getActiveCWindow :: State -> Maybe CWindow
getActiveCWindow state = fmap (Window.cWindow) $ getActiveWindow state

getBufferById :: State -> Buffer.BufferId -> Maybe Buffer.Buffer
getBufferById state bufferId = Map.lookup bufferId (buffers state)

getWindowBuffer :: State -> Window.Window -> Maybe Buffer.Buffer
getWindowBuffer state window = getBufferById state (Window.buffer window)

getActiveWindowAndBuffer :: State -> Maybe (Window.Window, Buffer.Buffer)
getActiveWindowAndBuffer state = do
    window <- getActiveWindow state
    buffer <- getBufferById state (Window.buffer window)
    return (window, buffer)

getActiveBuffer :: State -> Maybe Buffer.Buffer
getActiveBuffer state = do
    window <- getActiveWindow state
    buffer <- getBufferById state (Window.buffer window)
    return buffer

insertBuffer :: State -> Buffer.BufferId -> Buffer.Buffer -> State
insertBuffer state bufferId buffer = state {
    buffers = Map.insert bufferId buffer (buffers state)
}

replaceBuffer :: State -> Buffer.Buffer -> State
replaceBuffer state buffer = state {
    buffers = Map.insert (Buffer.bufferId buffer) buffer (buffers state)
}

setRegister :: State -> Text.Text -> Text.Text -> State
setRegister state register value = state {
    registers = Map.insert register value (registers state)
}

getRegister :: State -> Text.Text -> Text.Text
getRegister state register = case value of
    Nothing -> ""
    Just t  -> t
    where
        value = Map.lookup register (registers state)

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

setCursorPos :: State -> Window.Window -> Position -> State
setCursorPos state window pos =
    state {
        windows = Map.insert
            (Window.windowId window)
            (setScrollPos newWindow)
            (State.windows state)
    }
    where
        newWindow = window {Window.cursorPos = pos}

addToDot :: State -> Char -> State
addToDot state c = state {
    dotInput = Text.snoc (dotInput state) c
}
