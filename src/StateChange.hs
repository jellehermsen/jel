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

module StateChange where

import Control.Monad (guard)
import Data.Maybe (isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Char (isSpace)

import Types
import Helpers
import State
import qualified Window
import qualified Buffer

moveCursor :: State -> Position -> ChangedState
moveCursor state (0, 0) = Just (state, [])
moveCursor state dPos = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let countNewline = State.mode state `elem` [InsertMode, ReplaceMode, VisualMode]
    let pos = Buffer.closestPos buffer countNewline (addPos dPos (Window.cursorPos window))
    if (Window.cursorPos window) /= pos then do
            let newState = State.setCursorPos state window pos
            Just (newState, [])
        else Nothing

-- |Advances the cursor by one character, possibly putting it in a position
-- beyond what the current line length should allow. This is necessary, otherwise
-- you couldn't append to a line, or start typing on a previously empty line.
advanceCursor :: State -> ChangedState
advanceCursor state = do
    window <- getActiveWindow state
    let pos = Window.cursorPos window
    let newState = State.setCursorPos state window $ addPos pos (0, 1)
    Just (newState, [])

-- |Given a state and an action, this function does all the necessary changes
-- and gives you a new state and a list of events that bubbled up.
changeState :: State -> Action -> ChangedState

-- Move cursor down
changeState state (ActCursorDown n) = moveCursor state (n, 0)

-- Move cursor up
changeState state (ActCursorUp n) = moveCursor state (-n, 0)

-- Move cursor left
changeState state (ActCursorLeft n) = moveCursor state (0, -n)

-- Move cursor right
changeState state (ActCursorRight n) = moveCursor state (0, n)

-- Page down
changeState state (ActPageDown n) = do
    windowHeight <- fst <$> Window.size <$> getActiveWindow state
    moveCursor state ((quot windowHeight 2) * n, 0)

-- Page up
changeState state (ActPageUp n) = do
    windowHeight <- fst <$> Window.size <$> getActiveWindow state
    moveCursor state ((- quot windowHeight 2) * n, 0)

-- End of line
changeState state (ActEndOfLine) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let cursorPos = Window.cursorPos window
    line <- Buffer.lineForPos buffer cursorPos
    let changed = moveCursor state (0, Text.length line - (snd cursorPos))
    return $ fromMaybe (state, []) changed

-- Beginning of line
changeState state (ActBeginningOfLine) = do
   cols <- snd <$> Window.cursorPos <$> getActiveWindow state
   moveCursor state (0, -cols)

-- Move to first non-whitespace on the current line
changeState state (ActFirstNoneWhiteSpace) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let cursorPos = Window.cursorPos window
    line <- Buffer.lineForPos buffer cursorPos
    let whiteSpaceLength = Text.length $ Text.takeWhile (\x -> x == ' ' || x == '\t') line
    moveCursor state (0, -((snd cursorPos) - whiteSpaceLength))

changeState state (ActGotoLine n) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let cursorPos = Window.cursorPos window
    let newPos = Buffer.closestPos buffer False (n - 1, 0)
    moveCursor state $ subPos newPos cursorPos

changeState state (ActGotoLastLine) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let cursorPos = Window.cursorPos window
    let newPos = Buffer.closestPos buffer False (Buffer.lineCount buffer - 1, 0)
    moveCursor state $ subPos newPos cursorPos

changeState state (ActNextWord 0) = Just (state, [])
changeState state (ActNextWord n) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let pos@(row, col) = Window.cursorPos window
    ind <- nextWordIndex <$> Text.drop col <$> Buffer.lineForPos buffer pos
    guard (ind /= 0 || Buffer.lineCount buffer > row + 1)
    if ind == 0
        then do
            (newState, _) <- foldActions (Just (state, []))
                [ActCursorDown 1, ActFirstNoneWhiteSpace]
            changeState newState $ ActNextWord $ n - 1
        else do
             (newState, _) <- moveCursor state (0, ind)
             changeState newState $ ActNextWord $ n - 1

-- The ActPrevWord situation is a bit longer than I would like, but I have to
-- do some additional checks when moving up one row. You either have to move one
-- extra word to the left, or not, depending on whether you landed on a word of 1
-- character or more.
changeState state (ActPrevWord 0) = Just (state, [])
changeState state (ActPrevWord n) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let pos@(row, col) = Window.cursorPos window
    ind <- prevWordIndex <$> Text.take col <$> Buffer.lineForPos buffer pos
    guard (ind /= 0 || row > 0)
    if ind == 0
        then do
            (newState, _) <- foldActions (Just (state, []))
                [ActCursorUp 1, ActEndOfLine]
            (window2, buffer2) <- getActiveWindowAndBuffer newState
            line2 <- Buffer.lineForPos buffer2 $ Window.cursorPos window2
            if Text.length line2 > 1
                then do
                    let c1 = Text.index line2 $ Text.length line2 - 1
                    let c2 = Text.index line2 $ Text.length line2 - 2
                    if (isWordSeparator c1 == isWordSeparator c2)
                        then changeState newState $ ActPrevWord $ n
                        else changeState newState $ ActPrevWord $ n - 1
                else
                    changeState newState $ ActPrevWord $ n - 1
        else do
             (newState, _) <- moveCursor state (0, -ind)
             changeState newState $ ActPrevWord $ n - 1


changeState state (ActNextWordEnding 0) = Just (state, [])
changeState state (ActNextWordEnding n) = do
    offset <- wordEndOffset state
    if offset == 0 then do
        (newState, _) <- changeState state $ ActNextWord 1
        newOffset <- wordEndOffset newState
        (movedState, _) <- changeState newState $ (ActCursorRight newOffset)
        changeState movedState $ ActNextWordEnding $ n - 1
    else do
        (movedState, _) <- changeState state $ (ActCursorRight offset)
        changeState movedState $ ActNextWordEnding $ n - 1

changeState state (ActFlagUndoPoint) = do
    newBuffer <- Buffer.flagUndoPoint <$> getActiveBuffer state
    noEvents $ replaceBuffer state newBuffer

-- Switch to insert mode
changeState state (ActInsertMode) = do
    window <- getActiveWindow state
    guard $ not $ Window.readonly window
    noEvents $ state {mode = InsertMode}

-- Switch to replace mode
changeState state (ActReplaceMode) = do
    window <- getActiveWindow state
    guard $ not $ Window.readonly window
    noEvents $ state {mode = ReplaceMode}

-- Switch to command mode
-- Also reset the cursor position to an actual valid position. Insert mode
-- could have left the cursor in a position > the current line length.
changeState state (ActCommandMode) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let (row, col) = Buffer.closestPos buffer False $ Window.cursorPos window
    let newWindow = window { Window.cursorPos = (row, col)}
    let newState = state {
        windows = Map.insert
            (Window.windowId window)
            (setScrollPos newWindow)
            (State.windows state)
    }
    noEvents $ newState {mode = CommandMode}

-- Insert a character
changeState state (ActInsertChar c) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let cursorPos = Window.cursorPos window
    newBuffer <- Buffer.insertChar buffer cursorPos c True
    let newState = replaceBuffer state newBuffer
    movedCursor <- advanceCursor newState
    noEvents $ fst movedCursor

-- Insert a newline
changeState state (ActInsertNewLine) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let cursorPos = Window.cursorPos window
    newBuffer <- Buffer.splitLine buffer cursorPos True
    noEvents $ replaceBuffer state newBuffer

-- Delete characters
changeState state (ActDeleteChar n) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let cursorPos = Window.cursorPos window
    (newBuffer, _) <- Buffer.deleteText buffer cursorPos n True
    let countNewline = State.mode state `elem` [InsertMode, ReplaceMode, VisualMode]
    let pos = Buffer.closestPos newBuffer countNewline $ Window.cursorPos window
    noEvents $ State.setCursorPos (replaceBuffer state newBuffer) window pos

-- Delete characters before current position
changeState state (ActDeleteCharBefore n) = do
    movedState <- fst <$> changeState state (ActCursorLeft n)
    window <- getActiveWindow state
    movedWindow <- getActiveWindow movedState
    let pdiff = subPos (Window.cursorPos window) (Window.cursorPos movedWindow)
    changeState movedState $ ActDeleteChar $ abs $ getCol pdiff

-- Join lines
changeState state (ActJoinLine n) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let cursorPos = Window.cursorPos window
    newBuffer <- Buffer.joinLines buffer cursorPos n True
    let newState = replaceBuffer state newBuffer
    noEvents newState

-- Undo
changeState state (ActUndo n) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let cursorPos = Window.cursorPos window
    (newBuffer, newPos) <- Buffer.undo n cursorPos buffer
    noEvents $ State.setCursorPos (replaceBuffer state newBuffer) window newPos

-- Redo
changeState state (ActRedo n) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let cursorPos = Window.cursorPos window
    (newBuffer, newPos) <- Buffer.redo (n + 1) cursorPos buffer
    noEvents $ State.setCursorPos (replaceBuffer state newBuffer) window newPos

-- Delete sections
changeState state (ActDelete 0 _) = Just (state, [])

changeState state (ActDelete n [ActNextWord m]) =
    changeState state (ActDelete n [ActNextWordEnding m])

changeState state (ActDelete n [ActCursorRight m]) =
    changeState state (ActDeleteChar (n*m))

changeState state (ActDelete n [ActCursorLeft m]) =
    changeState state (ActDeleteCharBefore (n*m))

changeState state (ActDelete n [ActCursorDown m]) =
    changeState state (ActDeleteLine (n*m + 1))

changeState state (ActDelete n [ActCursorUp m]) = do
    (newState, _) <- changeState state (ActCursorUp m)
    changeState newState (ActDeleteLine (n*m + 1))

changeState state (ActDelete n motions) = do
    fromPos <- Window.cursorPos <$> getActiveWindow state
    let motionLen = length motions
    (changedState,_) <- foldActions (Just (state, []))
        $ take (n * motionLen) (cycle motions)
    (window, buffer) <- getActiveWindowAndBuffer changedState
    let toPos = Window.cursorPos window
    (newBuffer, _) <- Buffer.deleteSection buffer fromPos toPos
    let newPos = Buffer.closestPos newBuffer False (smallestPos toPos fromPos)
    noEvents $ State.setCursorPos (replaceBuffer state newBuffer) window newPos

-- Delete lines
changeState state (ActDeleteLine n) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let (row, col) = Window.cursorPos window
    let (targetRow, _) = Buffer.closestPos buffer False (row + n - 1, 0)
    (newBuffer, deletedLines) <- Buffer.deleteLines buffer row targetRow True
    let newPos = Buffer.closestPos newBuffer False (row, col)
    let newState = State.setRegister state "default" (Multi deletedLines True)
    noEvents $ State.setCursorPos (replaceBuffer newState newBuffer) window newPos

-- Find forward
changeState state (ActFindForward n c) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let pos@(row, col) = Window.cursorPos window
    line <- Buffer.lineForPos buffer pos
    index <- findNthIndex col n c line
    let newPos = Buffer.closestPos buffer False (row, col + index + 1)
    noEvents $ State.setCursorPos state window newPos

-- Find backward
changeState state (ActFindBackward n c) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let pos@(row, col) = Window.cursorPos window
    line <- Buffer.lineForPos buffer pos
    index <- findNthIndex (Text.length line - col - 1) n c (Text.reverse line)
    let newPos = Buffer.closestPos buffer False (row, col - index - 1)
    noEvents $ State.setCursorPos state window newPos

-- Replace character
changeState state (ActReplaceChar n c) = do
    (deletedState, _) <- changeState state (ActDeleteChar n)
    let replacement = Text.replicate n $ Text.singleton c
    (window, buffer) <- getActiveWindowAndBuffer deletedState
    let pos = Window.cursorPos window
    newBuffer <- Buffer.insertText buffer pos replacement True
    let newState = State.setCursorPos state window $ addPos pos $ (0, n - 1)
    noEvents $ replaceBuffer newState newBuffer

-- Replace character and advance cursor
changeState state (ActReplaceCharAndMove c) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let pos = Window.cursorPos window
    newBuffer <- Buffer.replaceChar buffer pos c True
    advanceCursor $ replaceBuffer state newBuffer

changeState state ActAdvanceCursor = advanceCursor state
changeState state ActRedrawScreen = Just (state, [EvRedrawScreen])
changeState _ ActIdle = Nothing
changeState _ (ActErrorMessage _) = Nothing
changeState state _ = Just (state, [EvQuit])

-- Helper functions

-- |Find out how many columns the cursor needs to move to the right in order to
-- reach the end of the current word.
wordEndOffset :: State -> Maybe Int
wordEndOffset state = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let pos@(_, col) = Window.cursorPos window
    text <- Text.drop col <$> Buffer.lineForPos buffer pos
    let char = Text.head text
    let len = Text.length $ Text.takeWhile (match char) (Text.tail text)
    if Text.length text == 0 || len == 0 then Just 0 else Just len
    where
        match c1 c2 =
            isWordSeparator c1 == isWordSeparator c2 && not (isSpace c2)

-- |Fold a bunch of actions resembling motions over a given ChangedState
foldActions :: ChangedState -> [Action] -> ChangedState
foldActions Nothing _ = Nothing
foldActions (Just (state, _)) [] = Just (state, [])
foldActions (Just (state, _)) (motion:xs) = do
    let changedState = changeState state motion
    if isNothing changedState then
        Just (state, [])
    else
        foldActions changedState xs

noEvents :: State -> ChangedState
noEvents state = Just (state, [])
