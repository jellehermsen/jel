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

import Helpers
import Types

type Marks = Map.Map Text.Text Types.Position
type BufferId = Id

data PastEvent = UndoFlag 
    | InsertText Types.Position Text.Text
    | DeleteText Types.Position Text.Text
    | ReplaceText Types.Position Text.Text Text.Text
    | SplitLine Types.Position
    | JoinLine Int
    | DeleteLine Int
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

lineForPos :: Buffer -> Position -> Maybe Text.Text
lineForPos buf (row, _) = Sequence.lookup row $ bLines buf

closestPos :: Buffer -> Position -> Position
closestPos buffer (row, col) = (closestRow, closestCol)
    where
        lengthRow = fromIntegral $ lineCount buffer
        closestRow = if row < 0
            then
                0
            else 
                if row >= lengthRow then
                    lengthRow - 1
                else
                    row
        line = Sequence.index (bLines buffer) $ fromIntegral closestRow
        closestCol = if col < 0
            then
                0
            else 
                if col >= (fromIntegral (Text.length line)) then
                    (fromIntegral $ Text.length line) - 1
                else
                    col 

insertText :: Buffer -> Position -> Text.Text -> Maybe Buffer
insertText buffer pos text = do
    line <- Buffer.lineForPos buffer pos
    let splitted = Text.splitAt (getCol pos) line
    let newLine = Text.concat [fst splitted, text, snd splitted]
    Just buffer {
        bLines = Sequence.update (getRow pos) newLine (bLines buffer),
        history = compressHistory $ InsertText pos text : history buffer
    }

insertChar :: Buffer -> Position -> Char -> Maybe Buffer
insertChar buffer pos c = insertText buffer pos (Text.singleton c)

deleteChar :: Buffer -> Position -> Int -> Maybe Buffer
deleteChar buffer pos n = Just buffer

flagUndoPoint :: Buffer -> Buffer
flagUndoPoint buffer = if hasHistory && lastEvent /= UndoFlag
    then
        buffer {history = UndoFlag : history buffer}
    else buffer {history = [UndoFlag]}
    where
        hasHistory = length (history buffer) > 0
        lastEvent = head $ history buffer

-- Tries to merge the last two PastEvents. This helps keep the Buffer history
-- small.
compressHistory :: [PastEvent] -> [PastEvent]
compressHistory [] = []
compressHistory (x:[]) = [x]
compressHistory (InsertText pos1 text1:InsertText pos2 text2:xs) =
    if subV2 pos1 pos2 == (0, Text.length text2) then
        InsertText pos2 (text2 `mappend` text1):xs
    else
        InsertText pos1 text1:InsertText pos2 text2 : xs
compressHistory xs = xs
