module Buffer where

import qualified Data.Text as Text
import qualified Types

type BufferId = Types.Id

data Buffer = Buffer
    { id :: Types.Id
    , lines   :: [Text.Text]
    , history :: Text.Text -- TODO
    , path    :: Text.Text
    }
    deriving (Show, Eq)


