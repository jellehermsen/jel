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

-- dummyText = Sequence.fromList ["UGH","    Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla vitae diam interdum, mollis orci eu, pharetra turpis. Suspendisse in imperdiet tellus. Fusce varius suscipit luctus. Maecenas posuere eget dolor non finibus. In eu dictum risus, ac posuere massa. Suspendisse et justo id augue consequat varius. Duis vitae massa odio. Nulla arcu ante, varius non dignissim in, aliquet non mauris. Curabitur eu odio gravida, faucibus enim sed, viverra justo. Sed volutpat id dolor ut tristique. Sed turpis augue, dignissim id varius at, sollicitudin at eros.", "Suspendisse elementum faucibus urna vel pharetra. Morbi non pretium ligula. Sed pharetra fermentum purus at fermentum. Proin dignissim dolor ac vehicula elementum. Nullam maximus elit quis ultrices accumsan. Phasellus auctor augue at egestas sollicitudin. Nulla facilisi. Phasellus rhoncus scelerisque tellus. Nulla faucibus dictum ligula, vitae ullamcorper dui eleifend vel. Aliquam pretium quam id risus eleifend, in semper ex lacinia. Nunc at eros tempor, lacinia felis eu, auctor est. Donec sit amet faucibus nulla.", "Curabitur nulla risus, auctor sed est sit amet, semper molestie enim. Vestibulum venenatis eu nisl a tincidunt. Aliquam ut nunc ut turpis lobortis tincidunt lacinia quis justo. Etiam convallis ex at elit vulputate scelerisque. Duis id lacinia nunc. Suspendisse ultricies lacus nibh, sit amet venenatis urna vestibulum vitae. Phasellus vel pellentesque mauris, quis pulvinar ante. Integer venenatis interdum risus, ac vestibulum lorem fringilla in.", "Fusce semper interdum orci, sed placerat turpis commodo non. Aenean accumsan auctor elit eget convallis. Proin blandit porta nulla sit amet lobortis. Vestibulum ac suscipit augue. Morbi et cursus sem, sit amet maximus urna. Pellentesque mollis felis et laoreet tincidunt. Ut posuere vel ex et volutpat. Sed et tincidunt nulla, ut auctor sapien. Ut nec aliquet felis. Suspendisse maximus faucibus consequat.", "Nam id viverra neque. Nullam pretium maximus lacus nec consequat. Sed malesuada molestie orci. Duis ex erat, rhoncus a feugiat laoreet, faucibus id mi. Fusce justo lorem, finibus in mauris viverra, porta auctor elit. Praesent id maximus nisl. Maecenas gravida porta leo, et pellentesque eros facilisis vestibulum. ", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "YIHAA"]
dummyText = Sequence.fromList ["Lorem ipsum."]

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
