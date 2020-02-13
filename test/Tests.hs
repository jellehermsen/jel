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
            "Move 1 character to the right: l",
            unp "l",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,1))
                return state
        ),
        (
            "Move 10 characters to the right: 10l",
            unp "10l",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,10))
                return state
        ),
        (
            "Move 1000 characters to the right: 1000l",
            unp "1000l",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,83))
                return state
        ),
        (
            "Move 1 line down: j",
            unp "j",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (1,0))
                return state
        ),
        (
            "Move 3 lines down: 3j",
            unp "3j",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (3,0))
                return state
        ),
        (
            "Move 1 line down and 1 line up: jk",
            unp "jk",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,0))
                return state
        ),
        (
            "Move 10 lines up: 10k",
            unp "10k",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,0))
                return state
        ),
        (
            "Move to the end of the line: $",
            unp "$",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,83))
                return state
        ),
        (
            "Move 1 line down and to the beginning: j0",
            unp "j0",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (1,0))
                return state
        ),
        (
            "Move 1 line down and to the first none-whitespace character: j^",
            unp "j^",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (1,4))
                return state
        ),
        (
            "Move to the end of the document: G",
            unp "G",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (105,0))
                return state
        ),
        (
            "Move to the end of the document and to the start: Ggg",
            unp "Ggg",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,0))
                return state
        ),
        (
            "Move to the next word: w",
            unp "w",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,6))
                return state
        ),
        (
            "Move to the 10th word: 10w",
            unp "10w",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,62))
                return state
        ),
        (
            "Move to the 100th word: 100w",
            unp "100w",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (6,60))
                return state
        ),
        (
            "Move to the end of the line and back 1 words: $b",
            unp "$b",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,79))
                return state
        ),
        (
            "Move to the end of the line and back 10 words: $b",
            unp "$10b",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (0,26))
                return state
        ),
        (
            "Move to the end of the document and back 100 words: G100b",
            unp "G100b",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (96,40))
                return state
        ),
        (
            "Move to line 50: 50gg",
            unp "50gg",
            \state -> do
                window <- State.getActiveWindow state
                guard (Window.cursorPos window == (49,0))
                return state
        ),
        (
            "Insert text: iTest",
            unp "iTest",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,4))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Text.take 5 line == "TestL"
                return state
        ),
        (
            "Delete a character: x",
            unp "x",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Text.take 4 line == "orem"
                return state
        ),
        (
            "Delete 4 characters: 4x",
            unp "4x",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Text.take 4 line == "m ip"
                return state
        ),
        (
            "Delete 1000 characters: 1000x",
            unp "1000x",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ line == ""
                return state
        ),
        (
            "Delete 1 line: dd",
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
            "Delete 10 lines: 10dd",
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
            "Delete 1000 lines: 1000dd",
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
            "Delete character: dl",
            unp "dl",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Text.take 4 line == "orem"
                return state
        ),
        (
            "Delete until 'm': dfm",
            unp "dfm",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Text.take 6 line == " ipsum"
                return state
        ),
        (
            "Delete until end of line: d$",
            unp "d$",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ line == ""
                return state
        ),
        (
            "Delete until end of next line: d2$",
            unp "d2$",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Text.take 5 line == "dolor"
                guard $ Buffer.lineCount buffer == 104
                return state
        ),
        (
            "Replace current character: rX",
            unp "rX",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Text.take 5 line == "Xorem"
                return state
        ),
        (
            "Replace 5 characters: 5rX",
            unp "5rX",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,4))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Text.take 5 line == "XXXXX"
                return state
        ),
        (
            "Insert text with Enter: iHAHA{ENTER}{ESC}",
            unp "iHAHA\n\ESC",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (1,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Buffer.lineCount buffer == 107
                guard $ line == "HAHA"
                return state
        ),
        (
            "Join 2 lines: J",
            unp "J",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Buffer.lineCount buffer == 105
                guard $ (Text.take 3 . Text.reverse) line == "des"
                return state
        ),
        (
            "Join 3 lines: 3J",
            unp "3J",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Buffer.lineCount buffer == 104
                guard $ (Text.take 3 . Text.reverse) line == "te "
                return state
        ),
        (
            "Join 1000 lines: 1000J",
            unp "1000J",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                line <- Buffer.lineForPos buffer (0,0)
                guard $ Buffer.lineCount buffer == 1
                guard $ (Text.take 3 . Text.reverse) line == ".xe"
                return state
        ),
        (
            "10 page downs: 10{PAGE_DOWN}",
            unp "10\EOT",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (105,0))
                return state
        ),
        (
            "10 page downs, 10 page ups: 10{PAGE_DOWN}10{PAGE_UP}",
            unp "10\EOT10\NAK",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                return state
        ),
        (
            "Find character: fi",
            unp "fi",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,6))
                return state
        ),
        (
            "Find character twice: 2fi",
            unp "2fi",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,19))
                return state
        ),
        (
            "Move to end of line and find character backwards: $Fq",
            unp "$Fq",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,79))
                return state
        ),
        (
            "Open line and type: oTEST{ESC}",
            unp "oTEST\ESC",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (1,3))
                guard $ Buffer.lineCount buffer == 107
                return state
        ),
        (
            "Split line: i{ENTER}{ESC}",
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
            "Delete all and undo, redo, undo: 1000dduru",
            unp "1000dduru",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                guard $ Buffer.lineCount buffer == 106
                return state
        ),
        (
            "Join all lines and undo, redo, undo: 1000Juru",
            unp "1000Juru",
            \state -> do
                (window, buffer) <- State.getActiveWindowAndBuffer state
                guard (Window.cursorPos window == (0,0))
                guard $ Buffer.lineCount buffer == 106
                return state
        )
    ]
