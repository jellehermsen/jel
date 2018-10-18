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
    , lastCommand :: [Command]
    , lastLine :: Text.Text
    , lastLineCWindow :: CWindow
    , lastLineHistory :: [Text.Text]
    , searchHistory :: [Text.Text]
    , screenSize :: (Integer, Integer)
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
    , lastCommand = []
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

insertBuffer :: State -> Buffer.BufferId -> Buffer.Buffer -> State
insertBuffer state bufferId buffer = state {
    buffers = Map.insert bufferId buffer (buffers state)
}
