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

import Control.Monad (foldM)
import qualified UI.NCurses as Curses
import qualified Data.Sequence as Sequence
import System.Environment (setEnv)

import Types
import qualified Buffer
import qualified Event
import qualified Gui
import qualified Input
import qualified State
import qualified StateChange
import qualified Window
import qualified Helpers

dummyText = Sequence.fromList ["Lorem ipsum.","Lorem ipsum." ,"Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum.","Lorem ipsum." ]

main :: IO ()
main = do
    setEnv "ESCDELAY" "0"
    Curses.runCurses $ do
        Curses.setEcho False
        Curses.setRaw True
        Curses.setCBreak True
        Curses.setCursorMode Curses.CursorVeryVisible

        motherCWindow <- Curses.defaultWindow
        (screenHeight, screenWidth) <- Gui.getWindowSize motherCWindow

        firstCWindow <- Curses.newWindow (toInteger (screenWidth - 1)) (toInteger screenWidth) 0 0
        let firstBuffer = (Buffer.newBuffer 0){Buffer.bLines = dummyText}
        let firstWindow = Window.newWindow 1 0 firstCWindow (screenHeight - 1, screenWidth)
        lastLineCWindow <- Curses.newWindow 1 (toInteger screenHeight) (toInteger (screenWidth - 1)) 0

        let state = State.insertBuffer (State.newState firstWindow lastLineCWindow (screenHeight, screenWidth)) 0 firstBuffer
        Gui.renderAll state
        Curses.render
        loop state motherCWindow

loop :: State.State -> CWindow -> Curses.Curses ()
loop state motherCWindow = do
    ev <- Curses.getEvent motherCWindow Nothing
    Helpers.traceMonad ev
    case ev of
        Nothing -> loop state motherCWindow
        Just ev' -> do
            -- Try to parse the input, retrieve a (partial) list of commands
            -- and a list of actions
            let parsedInput = Input.parseInput (State.mode state) (State.command state) ev'
            let command = either id (\_ -> []) parsedInput
            let actions = either (\_ -> []) id parsedInput

            -- Change the current command list in the state
            let newState = state {State.command = command}

            -- Change the state using the list of actions
            -- state <- Handler.handleActions newState actions
            let (state, events) = handleActions newState actions []
            foldM Event.handleEvent state events

            Gui.renderAll state
            Curses.render
            loop state motherCWindow

-- Changes the state using a list of actions, until one of the actions fail
handleActions :: State.State -> [Action] -> [Event] -> (State.State, [Event])
handleActions state [] events = (state, events)
handleActions state (action:actions) events = 
    case (StateChange.changeState state action) of
        Nothing -> (state, events)
        Just (newState, newEvents) -> 
            handleActions newState actions $ events ++ newEvents
