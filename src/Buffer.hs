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

import Control.Monad (guard)
import Data.Foldable (toList)
import qualified Data.Sequence as Sequence
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Helpers
import Types

-- ===========================
-- =         Types           =
-- ===========================
type Marks = Map.Map Text.Text Types.Position
type BufferId = Id
type Indentation = Int

-- |'PastEvent' is used to record previous mutations of a buffer so we can
-- undo/redo them.
data PastEvent = UndoFlag
    | InsertText Types.Position Text.Text
    | DeleteText Types.Position Text.Text
    | ReplaceText Types.Position Text.Text Text.Text
    | SplitLine Types.Position
    | DeleteLine Int Text.Text
    | InsertLine Int Text.Text
  deriving (Show, Eq)

data Buffer = Buffer
    { bufferId :: Id
    , bLines :: Sequence.Seq Text.Text
    , history :: [PastEvent]
    , path :: Text.Text
    , marks :: Marks
    , undoDepth :: Int
    } deriving (Show, Eq)

mkBuffer :: BufferId -> Buffer
mkBuffer bufferId' = Buffer
    { bufferId = bufferId'
    , bLines = Sequence.fromList [""]
    , history = []
    , path = ""
    , marks = Map.empty
    , undoDepth = 0
    }

-- ==========================
-- = Small helper functions =
-- ==========================

lineCount :: Buffer -> Int
lineCount buffer =  Sequence.length $ bLines buffer

lineForPos :: Buffer -> Position -> Maybe Text.Text
lineForPos buf (row, _) = Sequence.lookup row $ bLines buf

closestPos :: Buffer -> Position -> Position
closestPos buffer (row, col) = (closestRow, closestCol)
    where
        rowLength  = fromIntegral $ lineCount buffer
        line       = Sequence.index (bLines buffer) $ fromIntegral closestRow
        lineLength = fromIntegral (Text.length line)
        closestRow
            | row < 0           = 0
            | row >= rowLength  = rowLength - 1
            | otherwise         = row
        closestCol
            | col < 0           = 0
            | lineLength == 0   = 0
            | col >= lineLength = lineLength - 1
            | otherwise         = col

getLineRange :: Buffer -> Int -> Int -> Maybe (Sequence.Seq Text.Text)
getLineRange buffer from count
    | Sequence.length selection == 0 = Nothing
    | otherwise                      = Just selection
    where
        selection = Sequence.take count $ Sequence.drop from $ bLines buffer

-- =====================
-- = Buffer operations =
-- =====================

insertText :: Buffer -> Position -> Text.Text -> Bool -> Maybe Buffer
insertText buffer pos text updateHistory = do
    line <- lineForPos buffer pos
    let splitted = Text.splitAt (getCol pos) line
    let newLine = Text.concat [fst splitted, text, snd splitted]
    Just buffer {
        bLines = Sequence.update (getRow pos) newLine (bLines buffer),
        history = if updateHistory
            then
                compressHistory $ InsertText pos text : history buffer
            else
                history buffer
    }

insertChar :: Buffer -> Position -> Char -> Bool -> Maybe Buffer
insertChar buffer pos c = insertText buffer pos (Text.singleton c)

-- |Split a line into two different lines at the given position. Everything
-- right to the split point is inserted as a new line below the given one.
splitLine :: Buffer -> Position -> Bool -> Maybe Buffer
splitLine buffer pos updateHistory = do
    let row = getRow pos
    line <- lineForPos buffer pos
    let splitted = Text.splitAt (getCol pos) line
    let newLines = Sequence.insertAt (row + 1) (snd splitted)
                   $ Sequence.update row (fst splitted) (bLines buffer)

    Just buffer {
        bLines = newLines,
        history = if updateHistory
            then
                SplitLine pos : history buffer
            else
                history buffer
    }

-- |Make a number of lines starting from a given position and a row count into
-- one big line, intercalating them with a single "space".
joinLines :: Buffer -> Position -> Int -> Bool -> Maybe Buffer
joinLines buffer (row, _) count updateHistory = do
    selection <- getLineRange buffer row count
    let list = toList selection
    let newLine = Text.intercalate " " $ map Text.strip list
    let linesLeft = Buffer.lineCount buffer - row
    guard $ linesLeft > 1
    let realCount = if row + count > linesLeft then (linesLeft - row) else count
    (buf, _) <- deleteLines buffer row (row + realCount - 1) False
    return buf {
        bLines = Sequence.insertAt row newLine (bLines buf),
        history = if updateHistory
            then
                compressHistory $ InsertLine row newLine : history buf
            else
                history buf
    }

-- |Delete n characters on given position. If n == -1 it deletes all the
-- characters until the end of the line.
deleteText :: Buffer -> Position -> Int -> Bool -> Maybe (Buffer, Text.Text)
deleteText buffer pos@(row, col) n updateHistory = do
    line <- lineForPos buffer pos
    let deleteCount = if n == -1 then Text.length line - col else n
    if Text.length line == 0 then
        Nothing
    else do
        let (first, removed, second) = split3 (getCol pos) deleteCount line
        Just (
            buffer {
                bLines = Sequence.update
                    row
                    (Text.concat [first, second])
                    (bLines buffer)
                , history = if updateHistory
                    then
                        compressHistory $ DeleteText pos removed : history buffer
                    else
                        history buffer
            }, removed)

-- |Delete a section inside a buffer including the two positions.
deleteSection :: Buffer -> Position -> Position -> Maybe (Buffer, Text.Text)
deleteSection = deleteSection' ""

deleteSection' :: Text.Text -> Buffer -> Position -> Position -> Maybe (Buffer, Text.Text)
deleteSection' removed buffer pos1 pos2
    | pos1 == pos2 = Just (buffer, "")
    | fromRow == toRow = do
        l <- lineForPos buffer fromPos

        -- If this isn't the first removal and text on this line is removed all
        -- the way to the last column, we remove the entire line instead.
        if removed == "" || Text.length l > toCol + 1
            then deleteText buffer fromPos (toCol - fromCol + 1) True
            else deleteLine buffer fromRow True
    | otherwise = do
        let deleteline = fromCol == 0
        (b1, t1) <- if deleteline
            then
                deleteLine buffer fromRow True
            else
                deleteText buffer fromPos (-1) True
        let newRemoved = Text.intercalate "\n" [removed, t1]
        let nextFrom = if deleteline then (fromRow, 0) else (fromRow + 1, 0)
        let nextTo = if deleteline then (toRow - 1, toCol) else toPos
        deleteSection' newRemoved b1 nextFrom nextTo
    where
        fromPos@(fromRow, fromCol) = smallestPos pos1 pos2
        toPos@(toRow, toCol) = largestPos pos1 pos2

-- |'deleteLine' deletes the line at the given row. If you pass checkEmpty as
-- False it won't insert a new line after the last one has been removed. You
-- will have to handle that manually in that case, since there should always be
-- one line.
deleteLine :: Buffer -> Int -> Bool -> Maybe (Buffer, Text.Text)
deleteLine buffer row checkEmpty = do
    text <- lineForPos buffer (row, 0)
    let (newLines, newHistory) = if lineCount buffer == 1 && checkEmpty
        then (
                Sequence.fromList [""],
                (InsertLine 0 "") : (DeleteLine row text) : history buffer
            )
        else (
                Sequence.deleteAt row (bLines buffer),
                (DeleteLine row text) : history buffer
            )

    Just (buffer {
            bLines = newLines,
            history = newHistory
        }, text)

deleteLines :: Buffer -> Int -> Int -> Bool -> Maybe (Buffer, Text.Text)
deleteLines buffer row1 row2 checkEmpty = do
    let range = take (toRow - fromRow + 1) $ repeat fromRow
    (newBuffer, texts) <- deleteLines' (Just (buffer, [])) range checkEmpty
    return (newBuffer, Text.intercalate "\n" texts)
    where
        fromRow = min row1 row2
        toRow = max row1 row2

deleteLines' :: Maybe (Buffer, [Text.Text]) -> [Int] -> Bool -> Maybe (Buffer, [Text.Text])
deleteLines' Nothing _ _ = Nothing
deleteLines' x [] _ = x
deleteLines' (Just (buffer, texts)) (x:xs) checkEmpty = do
    (newBuffer, text) <- deleteLine buffer x checkEmpty
    deleteLines' (Just (newBuffer, text:texts)) xs checkEmpty

-- ===============
-- = Undo / Redo =
-- ===============

-- |Decrease the current undo depth. This is used after doing an "redo"
-- operation.
decUndoDepth :: Buffer -> Buffer
decUndoDepth buffer = buffer {undoDepth = undoDepth buffer - 1}

-- |Increase the current undo depth. This is used after doing an "undo"
-- operation.
incUndoDepth :: Buffer -> Buffer
incUndoDepth buffer = buffer {undoDepth = undoDepth buffer + 1}

-- |'redo' reverses a number of PastEvents of the buffer, updating the
-- undoDepth in the process.
redo :: Int -> Position -> Buffer -> Maybe (Buffer, Position)
redo 0 pos buffer = Just (buffer, pos)
redo count pos buffer = do
    let his = reverse $ take (undoDepth buffer) (history buffer)
    (newBuffer, newPosition) <- redoStep his $ Just (buffer, pos)
    redo (count - 1) newPosition newBuffer

redoStep :: [PastEvent] -> Maybe (Buffer, Position) -> Maybe (Buffer, Position)

redoStep _ Nothing = Nothing
redoStep [] (Just (buf, pos)) = Just (buf, pos)

redoStep (UndoFlag:_) (Just (buffer, pos)) = Just (decUndoDepth buffer, pos)

redoStep (InsertText pos text:xs) (Just (buffer, _)) = do
    newBuffer <- insertText buffer pos text False
    redoStep xs (Just (decUndoDepth newBuffer, pos))

redoStep (DeleteText pos text:xs) (Just (buffer, _)) = do
    (newBuffer, _) <- deleteText buffer pos (Text.length text) False
    redoStep xs (Just (decUndoDepth newBuffer, pos))

redoStep (InsertLine row text:xs) (Just (buffer, _)) = do
    let newBuffer = buffer {
        bLines = Sequence.insertAt row text (bLines buffer),
        undoDepth = (undoDepth buffer) - 1
    }
    redoStep xs (Just (newBuffer, (row, 0)))

redoStep (DeleteLine row _:xs) (Just (buffer, _)) = do
    let newBuffer = buffer {
        bLines = Sequence.deleteAt row (bLines buffer),
        undoDepth = (undoDepth buffer) - 1
    }
    redoStep xs (Just (newBuffer, (row, 0)))

redoStep (SplitLine pos@(row, col):xs) (Just (buffer, _)) = do
    newBuffer <- splitLine buffer pos False
    redoStep xs (Just (decUndoDepth newBuffer, (row + 1, col)))

redoStep _ (Just _) = Nothing

-- |'undo' is called with the amount of "undos", for example 10 when you do 10u
-- a cursor position and a buffer and it returns a tuple with the new buffer,
-- the new cursor position, or Nothing.
undo :: Int -> Position -> Buffer -> Maybe (Buffer, Position)
undo 0 pos buffer = Just (buffer, pos)
undo count pos buffer = do
    let his = drop (undoDepth buffer) (history buffer)
    (newBuffer, newPos) <- undoStep his (Just (buffer, pos))
    undo (count - 1) newPos newBuffer

undoStep :: [PastEvent] -> Maybe (Buffer, Position) -> Maybe (Buffer, Position)

undoStep _ Nothing = Nothing
undoStep [] (Just (buf, pos)) = Just (buf, pos)

undoStep (UndoFlag:_) (Just (buffer, pos)) = Just (incUndoDepth buffer, pos)

undoStep (DeleteText pos text:xs) (Just (buffer, _)) = do
    newBuffer <- insertText buffer pos text False
    undoStep xs (Just (incUndoDepth newBuffer, pos))

undoStep (InsertText pos text:xs) (Just (buffer, _)) = do
    (newBuffer, _) <- deleteText buffer pos (Text.length text) False
    undoStep xs (Just (incUndoDepth newBuffer, pos))

undoStep (DeleteLine row text:xs) (Just (buffer, _)) = do
    let newBuffer = buffer {
        bLines = Sequence.insertAt row text (bLines buffer),
        undoDepth = (undoDepth buffer) + 1
    }
    undoStep xs (Just (newBuffer, (row, 0)))

undoStep (InsertLine row _:xs) (Just (buffer, _)) = do
    let newBuffer = buffer {
        bLines = Sequence.deleteAt row (bLines buffer),
        undoDepth = (undoDepth buffer) + 1
    }
    undoStep xs (Just (newBuffer, (row, 0)))

undoStep (SplitLine pos@(row, _):xs) (Just (buffer, _)) = do
   line1 <- lineForPos buffer pos
   line2 <- lineForPos buffer (row + 1, 0)

   let newBuffer = buffer {
       bLines = Sequence.update (row) (Text.concat [line1, line2])
                $ Sequence.deleteAt (row + 1) (bLines buffer),
       undoDepth = (undoDepth buffer) + 1
   }

   undoStep xs (Just (newBuffer, (row, 0)))

undoStep _ (Just _) = Nothing

-- ===========
-- = History =
-- ===========

-- |Add a new undo flag to the Buffer's history. Whenever you undo something,
-- all the events up to an undo point are undone.
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

-- |Tries to merge the last two PastEvents. This helps keep the Buffer history
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
        DeleteText pos1 (text2 `mappend` text1) : xs
    else
        if subV2 pos1 pos2 == (0, -1) then
            DeleteText pos2 (text1 `mappend` text2) : xs
        else
            DeleteText pos1 text1 : DeleteText pos2 text2 : xs
compressHistory xs = xs
