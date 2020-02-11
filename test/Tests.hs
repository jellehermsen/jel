module Main where

import Prelude hiding (putStrLn, putStr)

import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import qualified UI.NCurses as Curses
import qualified Data.Sequence as Sequence
import qualified Data.Text as Text
import Data.Maybe (isJust)
import Data.Text.IO (putStrLn, putStr)
import System.Exit (exitFailure)

import Types
import Helpers

import qualified Buffer
import qualified Event
import qualified Gui
import qualified Input
import qualified State
import qualified StateChange
import qualified Window

type Test = (Text.Text, String, (State.State -> Maybe State.State))

testText = Sequence.fromList ["Lorem ipsum dolor sit amet(());, consectetur adipiscing elit. [Donec placerat, quam]", "\t    ....sed facilisis (porta), velit turpis elementum t t t erat, sed aliquam orci sapien sed", "dolor. Nullam p p p vestibulum ac felis a finibus. Donec condimentum dolor risus, et", "commodo nisi euismod sit amet. Ut tempus ligula sit amet eros feugiat", "vulputate. Suspendisse suscipit lacus eu lorem euismod commodo. Pellentesque", "habitant morbi tristique senectus et netus et malesuada fames ac turpis", "egestas. Cras at bibendum eros. Suspendisse mi diam, auctor ultricies malesuada", "vel, condimentum ut ante. Nullam ac orci molestie, mollis ante eu, tincidunt", "nunc. Sed vel imperdiet arcu, id semper purus. Curabitur rutrum ante in ante", "mattis aliquam. Fusce malesuada mattis tellus. Integer pulvinar urna et", "dignissim venenatis.", "", "Nam eget efficitur neque, id bibendum enim. Curabitur non enim non nulla", "fermentum malesuada in nec dolor. Ut ac consequat eros, ac molestie nibh. Nam", "elementum nulla non erat suscipit ullamcorper. Praesent ac gravida nibh.", "Integer convallis malesuada fermentum. Aenean libero ipsum, volutpat at", "tincidunt eu, mollis vel dolor. Etiam vel mattis lectus. Ut varius tempus massa", "finibus aliquam. Class aptent taciti sociosqu ad litora torquent per conubia", "nostra, per inceptos himenaeos. Donec tellus ante, cursus commodo blandit vel,", "faucibus non ante. Phasellus tempor, ligula eget convallis aliquam, mauris", "neque iaculis leo, nec mattis justo nibh a elit. Nunc nec felis ultrices,", "molestie justo quis, tincidunt mi. Mauris imperdiet accumsan convallis. Duis et", "tortor condimentum neque auctor convallis a eu leo.", "", "Nam a risus vel libero convallis pellentesque. Suspendisse egestas pharetra", "nisi, non cursus magna interdum a. Nam venenatis, nisi a bibendum cursus, enim", "massa gravida arcu, sit amet semper nisl arcu bibendum eros. Suspendisse", "efficitur turpis in lobortis suscipit. Vivamus gravida ante at rhoncus", "vehicula. Mauris nisl erat, scelerisque non augue eget, ultricies pharetra", "diam. Nullam vulputate accumsan lorem imperdiet condimentum. Mauris facilisis", "mi at felis scelerisque, eu iaculis dui dapibus. Donec eget dui quis tellus", "ultrices luctus. Mauris bibendum blandit risus in elementum. Sed sit amet", "lobortis nulla.", "", "Fusce congue convallis velit, eget porta nibh placerat eu. Morbi sagittis non", "purus vitae lacinia. Donec aliquam, tellus vitae fermentum ultricies, nisi sem", "molestie mauris, sit amet maximus eros libero a sem. Nam ut eleifend purus,", "suscipit semper metus. Proin est neque, elementum id nisi quis, eleifend semper", "turpis. Suspendisse et euismod mi. Mauris eu sodales ipsum. Phasellus turpis", "urna, dignissim quis placerat nec, sodales finibus dui. Donec gravida nisl", "lorem, eu tincidunt velit ullamcorper a. Suspendisse lacinia a mi ac fringilla.", "Suspendisse quis neque vel felis dignissim bibendum eget ac tortor. Morbi nec", "tincidunt metus, aliquam posuere sem. Donec vel ligula id nibh molestie", "fermentum. Nulla maximus gravida magna eget cursus. Curabitur suscipit feugiat", "sapien.", "", "Fusce id venenatis mi, vel interdum arcu. Donec nunc orci, tempor ac lectus", "pretium, viverra mattis nulla. Vivamus viverra fermentum enim. Quisque", "tristique, nisl in lobortis egestas, ipsum mauris accumsan eros, id commodo", "augue tellus eget nibh. Cras sed blandit purus. Nulla porttitor, velit ut", "fringilla porttitor, lacus urna efficitur lorem, sed ornare libero ex in odio.", "Nunc ornare, libero non ullamcorper ultrices, justo diam elementum magna, non", "luctus arcu nibh id lacus. Vestibulum posuere urna id lacus finibus, at dictum", "sem tempus. Aliquam purus risus, hendrerit eu nulla in, consectetur aliquet", "sapien. Morbi a nulla eget tellus consequat placerat ut sed massa. Mauris vitae", "turpis pellentesque, ullamcorper lorem at, efficitur dolor. Integer vitae", "ullamcorper nulla. Suspendisse eu purus consequat, blandit orci sed, efficitur", "augue. In ut purus nisi.", "", "Nullam egestas eget orci vel tempor. Pellentesque et metus in quam vestibulum", "fermentum. Fusce elit felis, tincidunt aliquet dui nec, tristique iaculis", "ligula. Donec velit nunc, volutpat non sagittis sed, eleifend accumsan justo.", "Integer id urna eleifend, tincidunt lectus id, rutrum est. Pellentesque", "facilisis, mi in suscipit efficitur, tortor risus fringilla justo, et aliquam", "ligula ipsum nec metus. Vestibulum sed elementum orci. Proin ipsum arcu,", "iaculis a ante quis, hendrerit congue ligula. Proin vitae accumsan neque, nec", "placerat urna. Donec sed accumsan tellus.", "", "Vivamus mattis diam in ante cursus pulvinar. Fusce pellentesque ullamcorper mi", "nec mollis. Mauris pretium pretium tempor. Nunc efficitur magna imperdiet erat", "condimentum, eget tristique leo interdum. Nullam molestie nec turpis sed", "semper. Morbi quis dui massa. Nam pharetra, magna eget interdum iaculis, nunc", "nibh bibendum ex, ut faucibus elit nisi in dui. Vivamus dignissim nisi dui, in", "rhoncus purus tincidunt vitae. Cras augue nisi, consectetur porttitor posuere", "non, iaculis ut lacus. Phasellus imperdiet, massa et tempor lacinia, mi quam", "efficitur nisi, sit amet aliquet massa tortor vitae diam. Sed dapibus est quis", "pharetra aliquet. Cras sapien dolor, vestibulum vitae porttitor ut, viverra", "quis ex. Duis nec pellentesque dolor. Proin interdum tempor lorem, id vehicula", "nulla sodales varius. Quisque bibendum et diam id feugiat. Duis vehicula, justo", "ut consectetur vehicula, lorem metus tempor dui, eu accumsan nisi tellus eget", "risus.", "", "Vestibulum et mauris sed dui luctus mattis sed nec ante. Integer porttitor", "luctus diam, eget feugiat urna. Mauris efficitur nunc risus, rutrum commodo", "risus maximus vitae. Etiam pharetra sagittis nunc bibendum finibus. Suspendisse", "pulvinar a metus vel condimentum. Praesent quam mi, maximus ut finibus at,", "gravida eget risus. Integer ac metus tincidunt, auctor magna nec, porttitor", "tortor. Mauris ex nisl, congue in lectus non, ultricies interdum diam.", "", "Pellentesque at odio ornare, blandit velit ut, lacinia enim. Proin nisl massa,", "lacinia et sapien ac, ornare dapibus tortor. Quisque venenatis ullamcorper", "orci, volutpat tincidunt augue luctus nec. Vivamus ut tempus dolor. Sed pretium", "lacus at elit faucibus, in finibus erat lacinia. Nam id nibh id nibh semper", "tincidunt. Nam lacinia ligula nec est egestas varius. Vivamus lectus est,", "finibus auctor consectetur pharetra, tincidunt et augue. Curabitur at iaculis", "libero. Vivamus eleifend eros a urna vehicula, non vehicula tortor ultricies.", "Aenean non convallis dolor, id placerat augue. Mauris varius rutrum eros non", "blandit. Etiam fringilla, elit id ornare vestibulum, diam justo posuere augue,", "in pulvinar orci dui sit amet libero. Nulla facilisi.", "", "Quisque pharetra nunc enim, nec facilisis ligula accumsan vel. Vestibulum", "semper odio et feugiat commodo. Integer a ipsum ac velit ullamcorper pharetra.", "Nullam quis posuere erat. Lorem ipsum dolor sit amet, consectetur adipiscing", "elit. Aenean hendrerit nec neque vel venenatis. Nam quis feugiat lectus. Donec", "volutpat lectus sed bibendum aliquet. Ut et malesuada neque, pellentesque", "viverra ex."]
-- Lorem ipsum dolor sit amet(());, consectetur adipiscing elit. [Donec placerat, quam]
handleActions :: State.State -> [Action] -> State.State
handleActions state [] = state
handleActions state (action:actions) = 
    case (StateChange.changeState state action) of
        Nothing -> state
        Just (newState, _) -> 
            handleActions newState actions

runTest :: State.State -> String -> (State.State -> Maybe State.State) -> Bool
runTest state "" testFunc = isJust $ testFunc state
runTest state (char:chars) testFunc = runTest (handleActions newState actions) chars testFunc
    where
        parsedInput = Input.parseInput (State.mode state) 
            (State.command state) $ Curses.EventCharacter char
        command = either id (\_ -> []) parsedInput
        actions = either (\_ -> []) id parsedInput
        newState = state {State.command = command}

runTests :: [Test] -> State.State -> [(Text.Text, Bool)]
runTests [] _ = []
runTests (test:tests) state = (label, runTest state input testFunc):(runTests tests state)
    where
        (label,input,testFunc) = test

unp = Text.unpack

green :: IO ()
green = putStr "\x1b[32m"

red :: IO ()
red = putStr "\x1b[31m"

resetColor :: IO ()
resetColor = putStr "\x1b[0m"

printResult :: (Text.Text, Bool) -> IO ()
printResult (label, True) = do
    green
    putStrLn $ Text.concat ["✓ ", label]
    resetColor

printResult (label, False) = do
    red
    putStrLn $ Text.concat ["✕ ", label]
    resetColor

main :: IO ()
main = do
    success <- Curses.runCurses $ do
        firstCWindow <- Curses.defaultWindow
        let firstBuffer = (Buffer.newBuffer 0){Buffer.bLines = testText}
        let firstWindow = Window.newWindow 1 0 firstCWindow (24, 80)
        lastLineCWindow <- Curses.defaultWindow
        let state = State.insertBuffer (State.newState firstWindow lastLineCWindow (25, 80)) 0 firstBuffer
        let results = runTests tests state
        liftIO $ putStrLn "--------------------------------"
        liftIO $ mapM_ printResult results
        liftIO $ putStrLn "--------------------------------"
        let passed = filter (\x -> snd x == True) results
        return $ length passed == length results

    if success then
        return ()
    else
        exitFailure

tests :: [Test]
tests = [
        (
            "Move 1 character to the right",
            unp "l",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,1))
                return state
        ),
        (
            "Move 10 characters to the right",
            unp "10l",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,10))
                return state
        ),
        ( "Move 1000 characters to the right",
            unp "1000l",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,83))
                return state
        ),
        ( "Move 1 line down",
            unp "j",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (1,0))
                return state
        ),
        ( "Move 3 lines down",
            unp "3j",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (3,0))
                return state
        ),
        ( "Move 1 line down and 1 line up",
            unp "jk",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,0))
                return state
        ),
        ( "Move 10 lines up",
            unp "10k",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,0))
                return state
        ),
        ( "Move to the end of the line",
            unp "$",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,83))
                return state
        ),
        ( "Move 1 line down and to the beginning",
            unp "j0",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (1,0))
                return state
        ),
        ( "Move 1 line down and to the first none-whitespace character",
            unp "j^",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (1,5))
                return state
        ),
        ( "Insert text",
            unp "iTest",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,4))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Text.take 5 line == "TestL"
                return state
        ),
        ( "Delete a character",
            unp "x",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Text.take 4 line == "orem"
                return state
        ),
        ( "Delete 4 characters",
            unp "4x",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Text.take 4 line == "m ip"
                return state
        ),
        ( "Delete 1000 characters",
            unp "1000x",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ line == ""
                return state
        ),
        ( "Delete 1 line",
            unp "dd",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Text.take 12 line == "\t    ....sed"
                guard $ Buffer.lineCount buffer == 105
                return state
        ),
        ( "Delete 10 lines",
            unp "10dd",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Text.take 9 line == "dignissim"
                guard $ Buffer.lineCount buffer == 96
                return state
        ),
        ( "Delete 1000 lines",
            unp "1000dd",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ line == ""
                guard $ Buffer.lineCount buffer == 1
                return state
        ),
        ( "Find character",
            unp "fi",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,6))
                return state
        ),
        ( "Find character twice",
            unp "2fi",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,19))
                return state
        ),
        ( "Move to end of line and find character backwards",
            unp "$Fq",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,79))
                return state
        )
    ]

