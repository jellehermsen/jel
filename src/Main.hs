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

dummyText = Sequence.fromList ["Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec placerat, quam", "sed facilisis porta, velit turpis elementum erat, sed aliquam orci sapien sed", "dolor. Nullam vestibulum ac felis a finibus. Donec condimentum dolor risus, et", "commodo nisi euismod sit amet. Ut tempus ligula sit amet eros feugiat", "vulputate. Suspendisse suscipit lacus eu lorem euismod commodo. Pellentesque", "habitant morbi tristique senectus et netus et malesuada fames ac turpis", "egestas. Cras at bibendum eros. Suspendisse mi diam, auctor ultricies malesuada", "vel, condimentum ut ante. Nullam ac orci molestie, mollis ante eu, tincidunt", "nunc. Sed vel imperdiet arcu, id semper purus. Curabitur rutrum ante in ante", "mattis aliquam. Fusce malesuada mattis tellus. Integer pulvinar urna et", "dignissim venenatis.", "", "Nam eget efficitur neque, id bibendum enim. Curabitur non enim non nulla", "fermentum malesuada in nec dolor. Ut ac consequat eros, ac molestie nibh. Nam", "elementum nulla non erat suscipit ullamcorper. Praesent ac gravida nibh.", "Integer convallis malesuada fermentum. Aenean libero ipsum, volutpat at", "tincidunt eu, mollis vel dolor. Etiam vel mattis lectus. Ut varius tempus massa", "finibus aliquam. Class aptent taciti sociosqu ad litora torquent per conubia", "nostra, per inceptos himenaeos. Donec tellus ante, cursus commodo blandit vel,", "faucibus non ante. Phasellus tempor, ligula eget convallis aliquam, mauris", "neque iaculis leo, nec mattis justo nibh a elit. Nunc nec felis ultrices,", "molestie justo quis, tincidunt mi. Mauris imperdiet accumsan convallis. Duis et", "tortor condimentum neque auctor convallis a eu leo.", "", "Nam a risus vel libero convallis pellentesque. Suspendisse egestas pharetra", "nisi, non cursus magna interdum a. Nam venenatis, nisi a bibendum cursus, enim", "massa gravida arcu, sit amet semper nisl arcu bibendum eros. Suspendisse", "efficitur turpis in lobortis suscipit. Vivamus gravida ante at rhoncus", "vehicula. Mauris nisl erat, scelerisque non augue eget, ultricies pharetra", "diam. Nullam vulputate accumsan lorem imperdiet condimentum. Mauris facilisis", "mi at felis scelerisque, eu iaculis dui dapibus. Donec eget dui quis tellus", "ultrices luctus. Mauris bibendum blandit risus in elementum. Sed sit amet", "lobortis nulla.", "", "Fusce congue convallis velit, eget porta nibh placerat eu. Morbi sagittis non", "purus vitae lacinia. Donec aliquam, tellus vitae fermentum ultricies, nisi sem", "molestie mauris, sit amet maximus eros libero a sem. Nam ut eleifend purus,", "suscipit semper metus. Proin est neque, elementum id nisi quis, eleifend semper", "turpis. Suspendisse et euismod mi. Mauris eu sodales ipsum. Phasellus turpis", "urna, dignissim quis placerat nec, sodales finibus dui. Donec gravida nisl", "lorem, eu tincidunt velit ullamcorper a. Suspendisse lacinia a mi ac fringilla.", "Suspendisse quis neque vel felis dignissim bibendum eget ac tortor. Morbi nec", "tincidunt metus, aliquam posuere sem. Donec vel ligula id nibh molestie", "fermentum. Nulla maximus gravida magna eget cursus. Curabitur suscipit feugiat", "sapien.", "", "Fusce id venenatis mi, vel interdum arcu. Donec nunc orci, tempor ac lectus", "pretium, viverra mattis nulla. Vivamus viverra fermentum enim. Quisque", "tristique, nisl in lobortis egestas, ipsum mauris accumsan eros, id commodo", "augue tellus eget nibh. Cras sed blandit purus. Nulla porttitor, velit ut", "fringilla porttitor, lacus urna efficitur lorem, sed ornare libero ex in odio.", "Nunc ornare, libero non ullamcorper ultrices, justo diam elementum magna, non", "luctus arcu nibh id lacus. Vestibulum posuere urna id lacus finibus, at dictum", "sem tempus. Aliquam purus risus, hendrerit eu nulla in, consectetur aliquet", "sapien. Morbi a nulla eget tellus consequat placerat ut sed massa. Mauris vitae", "turpis pellentesque, ullamcorper lorem at, efficitur dolor. Integer vitae", "ullamcorper nulla. Suspendisse eu purus consequat, blandit orci sed, efficitur", "augue. In ut purus nisi.", "", "Nullam egestas eget orci vel tempor. Pellentesque et metus in quam vestibulum", "fermentum. Fusce elit felis, tincidunt aliquet dui nec, tristique iaculis", "ligula. Donec velit nunc, volutpat non sagittis sed, eleifend accumsan justo.", "Integer id urna eleifend, tincidunt lectus id, rutrum est. Pellentesque", "facilisis, mi in suscipit efficitur, tortor risus fringilla justo, et aliquam", "ligula ipsum nec metus. Vestibulum sed elementum orci. Proin ipsum arcu,", "iaculis a ante quis, hendrerit congue ligula. Proin vitae accumsan neque, nec", "placerat urna. Donec sed accumsan tellus.", "", "Vivamus mattis diam in ante cursus pulvinar. Fusce pellentesque ullamcorper mi", "nec mollis. Mauris pretium pretium tempor. Nunc efficitur magna imperdiet erat", "condimentum, eget tristique leo interdum. Nullam molestie nec turpis sed", "semper. Morbi quis dui massa. Nam pharetra, magna eget interdum iaculis, nunc", "nibh bibendum ex, ut faucibus elit nisi in dui. Vivamus dignissim nisi dui, in", "rhoncus purus tincidunt vitae. Cras augue nisi, consectetur porttitor posuere", "non, iaculis ut lacus. Phasellus imperdiet, massa et tempor lacinia, mi quam", "efficitur nisi, sit amet aliquet massa tortor vitae diam. Sed dapibus est quis", "pharetra aliquet. Cras sapien dolor, vestibulum vitae porttitor ut, viverra", "quis ex. Duis nec pellentesque dolor. Proin interdum tempor lorem, id vehicula", "nulla sodales varius. Quisque bibendum et diam id feugiat. Duis vehicula, justo", "ut consectetur vehicula, lorem metus tempor dui, eu accumsan nisi tellus eget", "risus.", "", "Vestibulum et mauris sed dui luctus mattis sed nec ante. Integer porttitor", "luctus diam, eget feugiat urna. Mauris efficitur nunc risus, rutrum commodo", "risus maximus vitae. Etiam pharetra sagittis nunc bibendum finibus. Suspendisse", "pulvinar a metus vel condimentum. Praesent quam mi, maximus ut finibus at,", "gravida eget risus. Integer ac metus tincidunt, auctor magna nec, porttitor", "tortor. Mauris ex nisl, congue in lectus non, ultricies interdum diam.", "", "Pellentesque at odio ornare, blandit velit ut, lacinia enim. Proin nisl massa,", "lacinia et sapien ac, ornare dapibus tortor. Quisque venenatis ullamcorper", "orci, volutpat tincidunt augue luctus nec. Vivamus ut tempus dolor. Sed pretium", "lacus at elit faucibus, in finibus erat lacinia. Nam id nibh id nibh semper", "tincidunt. Nam lacinia ligula nec est egestas varius. Vivamus lectus est,", "finibus auctor consectetur pharetra, tincidunt et augue. Curabitur at iaculis", "libero. Vivamus eleifend eros a urna vehicula, non vehicula tortor ultricies.", "Aenean non convallis dolor, id placerat augue. Mauris varius rutrum eros non", "blandit. Etiam fringilla, elit id ornare vestibulum, diam justo posuere augue,", "in pulvinar orci dui sit amet libero. Nulla facilisi.", "", "Quisque pharetra nunc enim, nec facilisis ligula accumsan vel. Vestibulum", "semper odio et feugiat commodo. Integer a ipsum ac velit ullamcorper pharetra.", "Nullam quis posuere erat. Lorem ipsum dolor sit amet, consectetur adipiscing", "elit. Aenean hendrerit nec neque vel venenatis. Nam quis feugiat lectus. Donec", "volutpat lectus sed bibendum aliquet. Ut et malesuada neque, pellentesque", "viverra ex."]

main :: IO ()
main = do
    setEnv "ESCDELAY" "0"
    Curses.runCurses $ do
        Curses.setEcho False
        Curses.setRaw True
        Curses.setCBreak True
        Curses.setCursorMode Curses.CursorVeryVisible

        motherCWindow <- Curses.defaultWindow
        Curses.setKeypad motherCWindow True
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
            foldM (Event.handleEvent motherCWindow) state events

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
