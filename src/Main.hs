module Main where

-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"

import UI.NCurses

main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
        moveCursor 1 10
        drawString "Hello world!"
        moveCursor 3 10
        drawString "(press q to quit)"
        moveCursor 0 0
    render
    waitFor w

waitFor :: Window -> Curses ()
waitFor w = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> do
                case ev' of
                    EventCharacter 'l' -> do
                        cursor <- getCursor w
                        updateWindow w $ do
                            moveCursor (fst cursor) (snd cursor + 1)
                        render
                        loop
                    EventCharacter 'h' -> do
                        cursor <- getCursor w
                        updateWindow w $ do
                            moveCursor (fst cursor) (snd cursor - 1)
                        render
                        loop
                    EventCharacter 'k' -> do
                        cursor <- getCursor w
                        updateWindow w $ do
                            moveCursor (fst cursor - 1) (snd cursor)
                        render
                        loop
                    EventCharacter 'j' -> do
                        cursor <- getCursor w
                        updateWindow w $ do
                            moveCursor (fst cursor + 1) (snd cursor)
                        render
                        loop
                    EventCharacter 'q' -> return ()
                    otherwise -> loop
