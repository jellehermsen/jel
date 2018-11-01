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

import qualified Data.Sequence as Sequence
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
    , bLines :: Sequence.Seq Text.Text
    , history :: [PastEvent]
    , path :: Text.Text
    , marks :: Marks
    } deriving (Show, Eq)

newBuffer :: BufferId -> Buffer
newBuffer bufferId = Buffer
    { bufferId = bufferId
    , bLines = Sequence.fromList [""]
    , history = []
    , path = ""
    , marks = Map.empty
    }

lineCount :: Buffer -> Int
lineCount buffer =  Sequence.length $ bLines buffer

closestPos :: Buffer -> Position -> Position
closestPos buffer (y, x) = (closestY, closestX)
    where
        lengthY = fromIntegral $ lineCount buffer
        closestY = if y < 0 
            then 
                0
            else 
                if y >= lengthY then
                    lengthY - 1
                else
                    y
        line = Sequence.index (bLines buffer) $ fromIntegral closestY
        closestX = if x < 0
            then
                0
            else 
                if x >= (fromIntegral (Text.length line)) then
                    (fromIntegral $ Text.length line) - 1
                else
                    x 
