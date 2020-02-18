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

module Main where

import Prelude hiding (readFile, lines)

import Control.Monad (foldM)
import qualified UI.NCurses as Curses
import qualified Data.Sequence as Sequence
import System.Environment (setEnv)

-- Imports for testing
import Data.Text.IO (readFile)
import Data.Text (lines, unpack)
import Paths_jel

import Types
import qualified Buffer
import qualified Event
import qualified Gui
import qualified Input
import qualified State
import qualified StateChange
import qualified Window
import qualified Helpers

main :: IO ()
main = do
    setEnv "ESCDELAY" "0"
    filepath <- getDataFileName $ unpack "test.txt"
    contents <- readFile filepath
    let testText = Sequence.fromList $ lines contents
    Curses.runCurses $ do
        Curses.setEcho False
        Curses.setRaw True
        Curses.setCBreak True
        _ <- Curses.setCursorMode Curses.CursorVeryVisible

        motherCWindow <- Curses.defaultWindow
        Curses.setKeypad motherCWindow True
        (screenHeight, screenWidth) <- Gui.getWindowSize motherCWindow

        Curses.defineColor (Curses.Color 200) 150 150 150
        Curses.defineColor (Curses.Color 201) 500 500 500

        firstCWindow <- Curses.newWindow (toInteger (screenWidth - 1)) (toInteger screenWidth) 0 0
        let firstBuffer = (Buffer.mkBuffer 0){Buffer.bLines = testText}
        let firstWindow = Window.newWindow 1 0 firstCWindow (screenHeight - 1, screenWidth)
        lastLineCWindow <- Curses.newWindow 1 (toInteger screenHeight) (toInteger (screenWidth - 1)) 0

        let state = State.insertBuffer (State.mkState firstWindow lastLineCWindow (screenHeight, screenWidth)) 0 firstBuffer
        Gui.renderAll state
        Curses.render
        loop state motherCWindow

loop :: State.State -> CWindow -> Curses.Curses ()
loop state cwindow = do
    ev <- Curses.tryCurses $ Curses.getEvent cwindow Nothing
    _ <- Helpers.traceMonad ev
    case ev of
        Left _ -> loop state cwindow
        Right Nothing -> loop state cwindow
        Right (Just ev') -> do
            newState <- handleInput state ev' cwindow
            Gui.renderAll newState
            Curses.render
            loop newState cwindow

handleInput :: State.State -> Curses.Event -> CWindow -> Curses.Curses (State.State)
handleInput state ev cwindow = do
    let input = Input.parseInput (State.mode state) (State.command state) ev
    let command = either id (\_ -> []) input
    let actions = either (\_ -> []) id input
    let (newState, events) = handleActions (state {State.command = command}) actions []
    postEventsState <- foldM (Event.handleEvent cwindow) newState events
    return postEventsState

-- Changes the state using a list of actions, until one of the actions fail
handleActions :: State.State -> [Action] -> [Event] -> (State.State, [Event])
handleActions state [] events = (state, events)
handleActions state (action:actions) events = 
    case (StateChange.changeState state action) of
        Nothing -> (state, events)
        Just (newState, newEvents) -> 
            handleActions newState actions $ events ++ newEvents
