module Window where

import qualified Types
import qualified Buffer

data WindowType = File | Directory | Other
    deriving (Show, Eq)

type WindowId = Types.Id

data Window = Window
    { id :: WindowId
    , buffer :: Buffer.Buffer
    , scrollPos :: Types.Position
    , cursorPos :: Types.Position
    , readonly :: Bool
    , windowType :: WindowType
    }

