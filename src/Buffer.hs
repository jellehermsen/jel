module Buffer where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import qualified Types

type BufferId = Types.Id
type Marks = Map.Map Text.Text Types.Position

data Buffer = Buffer
    { id :: Types.Id
    , lines   :: [Text.Text]
    , history :: Text.Text -- TODO
    , path    :: Text.Text
    , marks   :: Marks
    }
    deriving (Show, Eq)


