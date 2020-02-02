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

import Control.Monad (foldM)
import Data.Maybe (isNothing)
import qualified Debug.Trace as Debug
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Types
import Helpers
import State
import qualified Window
import qualified Buffer


moveCursor :: State -> Position -> ChangedState
moveCursor state (0, 0) = Just (state, [])
moveCursor state dPos = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let (row, col) = Buffer.closestPos buffer (addPos dPos (Window.cursorPos window))
    let newWindow = window { Window.cursorPos = (row, col)}
    if (Window.cursorPos window) /= (row, col) then do
            let newState = state {
                windows = Map.insert
                    (Window.windowId window)
                    (setScrollPos newWindow)
                    (State.windows state)
            }
            Just (newState, [])
        else do
            Nothing

-- Advances the cursor by one character, possibly putting it in a position
-- beyond what the current line length should allow. This is necessary, otherwise
-- you couldn't append to a line, or start typing on a previously empty line.
advanceCursor :: State -> ChangedState
advanceCursor state = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let (row, col) = Window.cursorPos window
    let newWindow = window {Window.cursorPos = (row, col + 1)}
    let newState = state {windows = Map.insert (Window.windowId window) (setScrollPos newWindow) (State.windows state)}
    Just (newState, [])

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
    window <- getActiveWindow state
    let (windowHeight, _) = Window.size window
    moveCursor state ((quot windowHeight 2) * n, 0)

-- Page up
changeState state (ActPageUp n) = do
    window <- getActiveWindow state
    let (windowHeight, _) = Window.size window
    moveCursor state ((- quot windowHeight 2) * n, 0)

-- End of line
changeState state (ActEndOfLine) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let cursorPos = Window.cursorPos window
    line <- Buffer.lineForPos buffer cursorPos
    moveCursor state (0, Text.length line - (snd cursorPos))

-- Beginning of line
changeState state (ActBeginningOfLine) = do
   window <- getActiveWindow state
   let (_, cols) = Window.cursorPos window
   moveCursor state (0, -cols)

-- Move to first non-whitespace on the current line
changeState state (ActFirstNoneWhiteSpace) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let cursorPos = Window.cursorPos window
    line <- Buffer.lineForPos buffer cursorPos
    let whiteSpaceLength = Text.length $ Text.takeWhile (\x -> x == ' ' || x == '\t') line
    moveCursor state (0, -((snd cursorPos) - whiteSpaceLength))

changeState state (ActFlagUndoPoint) = do
    buffer <- getActiveBuffer state
    let newBuffer = Buffer.flagUndoPoint buffer
    return (replaceBuffer state newBuffer, [])

-- Switch to insert mode
changeState state (ActInsertMode) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    if Window.readonly window then
        Nothing
    else do
        let newState = replaceBuffer state buffer
        return (newState {mode = InsertMode}, [])

-- Switch to command mode
-- Also reset the cursor position to an actual valid position. Insert mode
-- could have left the cursor in a position > the current line length.
changeState state (ActCommandMode) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let (row, col) = Buffer.closestPos buffer $ Window.cursorPos window
    let newWindow = window { Window.cursorPos = (row, col)}
    let newState = state {
        windows = Map.insert
            (Window.windowId window)
            (setScrollPos newWindow)
            (State.windows state)
    }
    return (newState {mode = CommandMode}, [])

-- Insert a character
changeState state (ActInsertChar c) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let cursorPos = Window.cursorPos window
    newBuffer <- Buffer.insertChar buffer cursorPos c True
    let newState = replaceBuffer state newBuffer
    movedCursor <- advanceCursor newState
    return (fst movedCursor, [])

-- Insert a newline
changeState state (ActInsertNewLine) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let cursorPos = Window.cursorPos window
    newBuffer <- Buffer.splitLine buffer cursorPos True
    return (replaceBuffer state newBuffer, [])

-- Delete characters
changeState state (ActDeleteChar n) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let cursorPos = Window.cursorPos window
    (newBuffer, _) <- Buffer.deleteText buffer cursorPos n True
    let (row, col) = Buffer.closestPos newBuffer $ Window.cursorPos window
    let newWindow = window { Window.cursorPos = (row, col)}
    let newState = (replaceBuffer state newBuffer) {
        windows = Map.insert
            (Window.windowId window)
            (setScrollPos newWindow)
            (State.windows state)
    }
    return (newState, [])

-- Undo
changeState state (ActUndo n) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let cursorPos = Window.cursorPos window
    (newBuffer, newPos) <- Buffer.undo n cursorPos buffer
    let newWindow = window {Window.cursorPos = newPos}
    traceMonad (Buffer.history newBuffer)
    traceMonad (Buffer.undoDepth newBuffer)
    let newState = (replaceBuffer state newBuffer) {
        windows = Map.insert
            (Window.windowId window)
            (setScrollPos newWindow)
            (State.windows state)
    }

    return (replaceBuffer newState newBuffer, [])

-- Redo
changeState state (ActRedo n) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let cursorPos = Window.cursorPos window
    (newBuffer, newPos) <- Buffer.redo (n + 1) cursorPos buffer

    let newWindow = window {Window.cursorPos = Buffer.closestPos newBuffer newPos}
    let newState = (replaceBuffer state newBuffer) {
        windows = Map.insert
            (Window.windowId window)
            (setScrollPos newWindow)
            (State.windows state)
    }

    return (replaceBuffer newState newBuffer, [])

-- Delete sections
changeState state (ActDelete 0 motions) = Just (state, [])
changeState state (ActDelete n motions) = do
    oldWindow <- getActiveWindow state
    let fromPos = Window.cursorPos oldWindow

    let motionLen = length motions

    changedState <- foldMotions (Just (state, []))
                    $ take (n * motionLen) (cycle motions)

    (window, buffer) <- getActiveWindowAndBuffer $ fst changedState
    let toPos = Window.cursorPos window

    (newBuffer, removedText) <- 
        if (getRow fromPos == getRow toPos) then 
            Buffer.deleteSection buffer fromPos toPos
        else
            Buffer.deleteLines buffer (getRow fromPos) (getRow toPos)

    let newWindow = window {
        Window.cursorPos = (
            Buffer.closestPos newBuffer (smallestPos toPos fromPos)
        )
    }

    let newState = (replaceBuffer state newBuffer) {
        windows = Map.insert
            (Window.windowId window)
            (setScrollPos newWindow)
            (State.windows state)
    }
    return (newState, [])

-- Delete lines
changeState state (ActDeleteLine n) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let (row, col) = Window.cursorPos window
    let (targetRow, _) = Buffer.closestPos buffer (row + n - 1, 0)

    (newBuffer, removedText) <-
        Buffer.deleteLines buffer row targetRow

    let newWindow = window {
        Window.cursorPos = (
            Buffer.closestPos newBuffer (row, col)
        )
    }

    let newState = (replaceBuffer state newBuffer) {
        windows = Map.insert
            (Window.windowId window)
            (setScrollPos newWindow)
            (State.windows state)
    }
    return (newState, [])

-- Repeat
changeState state (ActRepeat n) = do
    (window, buffer) <- getActiveWindowAndBuffer state
    let pos = Window.cursorPos window
    return (state, [])

changeState state ActAdvanceCursor = advanceCursor state
changeState state ActRedrawScreen = Just (state, [EvRedrawScreen])
changeState state ActIdle = Nothing
changeState state (ActErrorMessage t) = Nothing
changeState state _ = Just (state, [EvQuit])

foldMotions :: ChangedState -> [Action] -> ChangedState
foldMotions Nothing _ = Nothing
foldMotions (Just (state, _)) [] = Just (state, [])
foldMotions (Just (state, _)) (motion:xs) = do
    let changedState = changeState state motion
    if isNothing changedState then
        Just (state, [])
    else
        foldMotions changedState xs
