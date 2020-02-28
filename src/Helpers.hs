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

module Helpers where

import qualified Debug.Trace as Debug
import qualified Data.Text as Text
import Data.Char (isAlphaNum, isSpace)
import System.IO.Unsafe()
import Types

addPos :: Position -> Position -> Position
addPos (row1, col1) (row2, col2) = (row1 + row2, col1 + col2)

subPos :: Position -> Position -> Position
subPos (row1, col1) (row2, col2) = (row1 - row2, col1 - col2)

getRow :: Position -> Int
getRow = fst

getCol :: Position -> Int
getCol = snd

smallestPos :: Position -> Position -> Position
smallestPos (row1, col1) (row2, col2)
    | row1 < row2                  = (row1, col1)
    | row1 == row2 && col1 <= col2 = (row1, col1)
    | otherwise                    = (row2, col2)

largestPos :: Position -> Position -> Position
largestPos (row1, col1) (row2, col2)
    | row1 < row2                  = (row2, col2)
    | row1 == row2 && col1 <= col2 = (row2, col2)
    | otherwise                    = (row1, col1)

posInRange :: Position -> V4 -> Bool
posInRange (row, col) (fromRow, fromCol, toRow, toCol) = 
    row >= fromRow && row <= toRow
    &&
    col >= fromCol && col <= toCol

posDiff :: Position -> V4 -> V2
posDiff (row, col) (fromRow, fromCol, toRow, toCol) =
    (
        if row < fromRow then row - fromRow else if row > toRow then row - toRow else 0,
        if col < fromCol then col - fromCol else if col > toCol then col - toCol else 0
    )

addV2 :: V2 -> V2 -> V2
addV2 (a, b) (c, d) = (a+c, b+d)

subV2 :: V2 -> V2 -> V2
subV2 (a, b) (c, d) = (a-c, b-d)

insertChar :: Int -> Char -> Text.Text -> Text.Text
insertChar pos c t =
    let
        splitted = Text.splitAt pos t
    in
       Text.concat [fst splitted, Text.singleton c, snd splitted]

-- |'findNthIndex' finds the first occurence of the given character after the
-- given column and amount of hits to skip
findNthIndex :: Int -> Int -> Char -> Text.Text -> Maybe Int
findNthIndex = findNthIndex' 0

findNthIndex' :: Int -> Int -> Int -> Char -> Text.Text -> Maybe Int
findNthIndex' total _ 0 _ _ = Just $ total - 1
findNthIndex' total col n c text = do
    let shortenedText = Text.drop (col + 1) text
    index <- Text.findIndex (\x -> x == c) shortenedText
    findNthIndex' (total + index + 1) index (n - 1) c shortenedText

-- Functions for debugging
traceMonad :: (Show a, Monad m) => a -> m a
traceMonad x = Debug.trace (show x) (return x)

trace :: Text.Text -> a -> a
trace = Debug.trace . Text.unpack

traceShow :: Show a => a -> b -> b
traceShow = Debug.traceShow

-- |Split text in 3 parts given the length of the first part, and the length of
-- the second
split3 :: Int -> Int -> Text.Text -> (Text.Text, Text.Text, Text.Text)
split3 len1 len2 t = (first, removed, second)
    where
        (first, rest) = Text.splitAt len1 t
        (removed, second) = Text.splitAt len2 rest

-- |'isWordSeparator' checks whether the given character is not a letter, digit or
-- underscore, this takes UTF-8 letters into account
isWordSeparator :: Char -> Bool
isWordSeparator c = isAlphaNum c || c == '_'

nextWordIndex :: Text.Text -> Int
nextWordIndex t
    | Text.length t == 0 = 0
    | Text.head t == ' ' = if (spaces t == Text.length t) then 0 else (spaces t + 1)
    | otherwise          = if (total == Text.length t) then 0 else total
    where
        spaces wsp      = Text.length $ Text.takeWhile isSpace wsp
        isSep           = isWordSeparator $ Text.head t
        notSpace        = not . isSpace
        (firstWord, t2) = Text.span (\c -> isWordSeparator c == isSep && notSpace c) t
        empty           = Text.takeWhile isSpace t2
        total           = Text.length firstWord + Text.length empty

prevWordIndex :: Text.Text -> Int
prevWordIndex t = movedChars
    where
        notSpace         = not . isSpace
        notSep           = not . isWordSeparator
        (spaces, t1)     = Text.span isSpace $ Text.reverse t
        separation       = Text.takeWhile (\c -> notSep c && notSpace c) t1
        firstWord        = Text.takeWhile (\c -> isWordSeparator c && notSpace c) t1
        movedChars
            | Text.length firstWord == 0 =
                Text.length spaces + Text.length separation
            | otherwise =
                Text.length spaces + Text.length firstWord

safeCycle :: [a] -> [a]
safeCycle [] = []
safeCycle xs = cycle xs

isSingle :: Register -> Bool
isSingle (Single _) = True
isSingle _ = False

fromSingle :: Register -> Text.Text
fromSingle (Single t) = t
fromSingle _ = ""

fromMulti :: Register -> [Text.Text]
fromMulti (Multi xs _) = xs
fromMulti _ = []

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
