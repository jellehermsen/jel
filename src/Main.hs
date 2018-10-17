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

import qualified UI.NCurses as Curses

import Types
import qualified Buffer
import qualified Gui
import qualified Handler
import qualified Input
import qualified State
import qualified Window

main :: IO ()
main = Curses.runCurses $ do
    Curses.setEcho False
    Curses.setCursorMode Curses.CursorVeryVisible
    motherWindow <- Curses.defaultWindow
    screenSize <- Gui.getWindowSize motherWindow
    firstWindow <- Curses.newWindow (snd screenSize - 1) (snd screenSize) 0 0
    lastWindow <- Curses.newWindow 1 (snd screenSize) (fst screenSize - 1) 0
    Curses.updateWindow lastWindow $ do
        Curses.moveCursor 0 0
        Curses.drawString "Boeiaka"
        Curses.drawString "YIHAAA"
    Curses.updateWindow firstWindow $ do
        Curses.moveCursor 1 10
        Curses.drawString "Hello world!" 
    let state = State.newState firstWindow lastWindow
    Curses.render
    loop state motherWindow

loop :: State.State -> CWindow -> Curses.Curses ()
loop state motherWindow = do
    ev <- Curses.getEvent motherWindow Nothing
    case ev of
        Nothing -> loop state motherWindow
        Just ev' -> do
            let (command, actions) = Input.parseInput (State.mode state) (State.command state) ev'
            let newState = state {State.command = command}
            state <- Handler.handleActions newState actions
            loop state motherWindow

{-
case ev' of
    Curses.EventCharacter 'q' -> return ()
    Curses.EventCharacter '\ESC' -> return ()
    otherwise -> do
        Curses.updateWindow motherWindow $ do
            Curses.moveCursor 0 0
            Curses.drawString $ show ev' 
        Curses.render
        loop state motherWindow
-}
