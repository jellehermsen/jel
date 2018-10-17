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

module Buffer where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Types

type Marks = Map.Map Text.Text Types.Position
type BufferId = Id
 
data PastEvent = UndoFlag 
    | NewText Types.Position Text.Text
    | ClearText Types.Position Text.Text
  deriving (Show, Eq)

data Buffer = Buffer
    { bufferId :: Id
    , bLines :: [Text.Text]
    , history :: [PastEvent]
    , path :: Text.Text
    , marks :: Marks
    } deriving (Show, Eq)

newBuffer :: BufferId -> Buffer
newBuffer bufferId = Buffer
    { bufferId = bufferId
    , bLines = [""]
    , history = []
    , path = ""
    , marks = Map.empty
    }
