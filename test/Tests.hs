module Main where

import Prelude hiding (putStrLn, putStr, readFile)

import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import qualified UI.NCurses as Curses
import qualified Data.Sequence as Sequence
import qualified Data.Text as Text
import Data.Maybe (isJust)
import Data.Text.IO (putStrLn, putStr, readFile)
import System.Exit (exitFailure)
import Paths_jel

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
    filepath <- getDataFileName $ unp "test.txt"
    contents <- readFile filepath
    let testText = Sequence.fromList $ Text.lines contents
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
        (
            "Move 1000 characters to the right",
            unp "1000l",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,83))
                return state
        ),
        (
            "Move 1 line down",
            unp "j",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (1,0))
                return state
        ),
        (
            "Move 3 lines down",
            unp "3j",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (3,0))
                return state
        ),
        (
            "Move 1 line down and 1 line up",
            unp "jk",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,0))
                return state
        ),
        (
            "Move 10 lines up",
            unp "10k",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,0))
                return state
        ),
        (
            "Move to the end of the line",
            unp "$",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,83))
                return state
        ),
        (
            "Move 1 line down and to the beginning",
            unp "j0",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (1,0))
                return state
        ),
        (
            "Move 1 line down and to the first none-whitespace character",
            unp "j^",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (1,4))
                traceMonad "UGH"
                return state
        ),
        (
            "Insert text",
            unp "iTest",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,4))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Text.take 5 line == "TestL"
                return state
        ),
        (
            "Delete a character",
            unp "x",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Text.take 4 line == "orem"
                return state
        ),
        (
            "Delete 4 characters",
            unp "4x",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Text.take 4 line == "m ip"
                return state
        ),
        (
            "Delete 1000 characters",
            unp "1000x",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ line == ""
                return state
        ),
        (
            "Delete 1 line",
            unp "dd",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Text.take 11 line == "\t   ....sed"
                guard $ Buffer.lineCount buffer == 105
                return state
        ),
        (
            "Delete 10 lines",
            unp "10dd",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Text.take 9 line == "dignissim"
                guard $ Buffer.lineCount buffer == 96
                return state
        ),
        (
            "Delete 1000 lines",
            unp "1000dd",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ line == ""
                guard $ Buffer.lineCount buffer == 1
                return state
        ),
        (
            "10 page downs",
            unp "10\EOT",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (105,0))
                return state
        ),
        (
            "10 page downs, 10 page ups",
            unp "10\EOT10\NAK",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                return state
        ),
        (
            "Find character",
            unp "fi",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,6))
                return state
        ),
        (
            "Find character twice",
            unp "2fi",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,19))
                return state
        ),
        (
            "Move to end of line and find character backwards",
            unp "$Fq",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,79))
                return state
        ),
        (
            "Open line and type",
            unp "oTEST\ESC",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (1,3))
                guard $ Buffer.lineCount buffer == 107
                return state
        ),
        (
            "Split line",
            unp "i\n\ESC",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (1,0))
                guard $ Buffer.lineCount buffer == 107
                line1 <- Buffer.lineForPos buffer (0,0)
                guard $ line1 == ""
                line2 <- Buffer.lineForPos buffer (1,0)
                guard $ Text.take 5 line2 == "Lorem"
                return state
        ),
        (
            "Delete all and undo",
            unp "1000dduru",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                guard $ Buffer.lineCount buffer == 106
                return state
        )
    ]
