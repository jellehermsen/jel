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
type Indentation = Int

data PastEvent = UndoFlag
    | InsertText Types.Position Text.Text
    | DeleteText Types.Position Text.Text
    | ReplaceText Types.Position Text.Text Text.Text
    | SplitLine Types.Position
    | JoinLine Int Text.Text
    | DeleteLine Int Text.Text
  deriving (Show, Eq)

data Buffer = Buffer
    { bufferId :: Id
    , bLines :: Sequence.Seq Text.Text
    , history :: [PastEvent]
    , path :: Text.Text
    , marks :: Marks
    , undoDepth :: Int
    } deriving (Show, Eq)

newBuffer :: BufferId -> Buffer
newBuffer bufferId = Buffer
    { bufferId = bufferId
    , bLines = Sequence.fromList [""]
    , history = []
    , path = ""
    , marks = Map.empty
    , undoDepth = 0
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
        lineLength = fromIntegral (Text.length line)
        closestCol = if col < 0
            then
                0
            else 
                if lineLength == 0 then
                    0
                else
                    if col >= lineLength then
                        lineLength - 1
                    else
                        col

insertText :: Buffer -> Position -> Text.Text -> Maybe Buffer
insertText buffer pos text = do
    line <- lineForPos buffer pos
    let splitted = Text.splitAt (getCol pos) line
    let newLine = Text.concat [fst splitted, text, snd splitted]
    Just buffer {
        bLines = Sequence.update (getRow pos) newLine (bLines buffer),
        history = compressHistory $ InsertText pos text : history buffer
    }

insertChar :: Buffer -> Position -> Char -> Maybe Buffer
insertChar buffer pos c = insertText buffer pos (Text.singleton c)

splitLine :: Buffer -> Position -> Maybe Buffer
splitLine buffer pos = do
    let row = getRow pos
    line <- lineForPos buffer pos
    let splitted = Text.splitAt (getCol pos) line
    let newLine = fst splitted

    let newLines = Sequence.insertAt (row + 1) (snd splitted)
                   $ Sequence.update row (fst splitted) (bLines buffer)

    Just buffer {
        bLines = newLines,
        history = SplitLine pos : history buffer
    }

deleteText :: Buffer -> Position -> Int -> Maybe Buffer
deleteText buffer pos n = do
    line <- lineForPos buffer pos
    if Text.length line == 0 then
        Nothing
    else do
        let (first, removed, second) = split3 (getCol pos) n line
        Just buffer {
            bLines = Sequence.update
                (getRow pos)
                (Text.concat [first, second])
                (bLines buffer)
            , history = compressHistory $ DeleteText pos removed : history buffer
        }

flagUndoPoint :: Buffer -> Buffer
flagUndoPoint buffer = if hasHistory && lastEvent /= UndoFlag
    then
        buffer {
            history = UndoFlag : (drop depth (history buffer)),
            undoDepth = 0
        }
    else buffer {history = [UndoFlag]}
    where
        hasHistory = length (history buffer) > 0
        depth = undoDepth buffer
        lastEvent = head $ history buffer

redo :: Int -> Buffer -> Maybe Buffer
redo 0 buffer = Just buffer
redo count buffer = do
    let his = reverse $ take (undoDepth buffer) (history buffer)
    newBuffer <- redoStep his (Just buffer)
    redo (count - 1) newBuffer

redoStep :: [PastEvent] -> Maybe Buffer -> Maybe Buffer
redoStep _ Nothing = Nothing
redoStep [] buf = buf

redoStep (UndoFlag:xs) (Just buffer) = Just $
    buffer {undoDepth = (undoDepth buffer) - 1}

redoStep (InsertText pos text:xs) (Just buffer) = do
    line <- lineForPos buffer pos
    let splitted = Text.splitAt (getCol pos) line
    let newLine = Text.concat [fst splitted, text, snd splitted]
    Just buffer {
        bLines = Sequence.update (getRow pos) newLine (bLines buffer),
        undoDepth = (undoDepth buffer) - 1
    }

undo :: Int -> Buffer -> Maybe Buffer
undo 0 buffer = Just buffer
undo count buffer = do
    let his = drop (undoDepth buffer) (history buffer)
    newBuffer <- undoStep his (Just buffer)
    undo (count - 1) newBuffer

undoStep :: [PastEvent] -> Maybe Buffer -> Maybe Buffer
undoStep _ Nothing = Nothing
undoStep [] buf = buf 

undoStep (UndoFlag:xs) (Just buffer) = Just $ 
    buffer {undoDepth = (undoDepth buffer) + 1}

undoStep (InsertText pos text:xs) (Just buffer) = do
    line <- lineForPos buffer pos
    let (first, removed, second) = split3 (getCol pos) (Text.length text) line
    if removed == text then
        undoStep xs $ Just buffer {
            bLines = Sequence.update
                (getRow pos)
                (Text.concat [first, second])
                (bLines buffer)
            , undoDepth = (undoDepth buffer) + 1
        }
    else
        Nothing

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

compressHistory (DeleteText pos1 text1:DeleteText pos2 text2:xs) =
    if pos1 == pos2 then
        DeleteText pos1 (text1 `mappend` text2) : xs
    else
        if subV2 pos1 pos2 == (0, -1) then
            DeleteText pos2 (text1 `mappend` text2) : xs
        else
            DeleteText pos1 text1 : DeleteText pos2 text2 : xs

compressHistory xs = xs
