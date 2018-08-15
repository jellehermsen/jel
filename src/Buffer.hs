module Buffer where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import qualified Types

type BufferId = Types.Id
type Marks = Map.Map Text.Text Types.Position

data PastEvent = UndoFlag 
    | NewText Types.Position Text.Text
    | ClearText Types.Position Text.Text
  deriving (Show, Eq)

data Buffer = Buffer
    { id :: Types.Id
    , lines :: [Text.Text]
    , history :: [PastEvent]
    , path :: Text.Text
    , marks :: Marks
    , lastCommand :: Text.Text -- TODO
    } deriving (Show, Eq)
