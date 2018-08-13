module State where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import qualified Window
import qualified Buffer
import qualified Types

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
    , mode :: Types.Mode
    , lastWindowId :: Window.WindowId
    , stateLine :: Text.Text
    , stateLineHistory :: [Text.Text]
    , searchHistory :: [Text.Text]
    }
