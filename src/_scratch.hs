{-
main :: IO ()
main = Curses.runCurses $ do
    Curses.setEcho False
    w <- Curses.defaultWindow
    cid <- Curses.newColorID Curses.ColorYellow Curses.ColorBlue 1
    Curses.updateWindow w $ do
        Curses.moveCursor 1 10
        Curses.drawString "Hello world!"
        Curses.moveCursor 3 10
        Curses.drawString "(press q to quit)"
        Curses.moveCursor 0 40 
        Curses.drawLineV (Just Curses.glyphLineV) 1000
        Curses.moveCursor 0 0
        Curses.setColor cid
        Curses.drawString "Test this"
    Curses.render
    waitFor w

waitFor :: Curses.Window -> Curses.Curses ()
waitFor w = loop where
    loop = do
        ev <- Curses.getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> do
                case ev' of
                    Curses.EventCharacter 'l' -> do
                        cursor <- Curses.getCursor w
                        Curses.updateWindow w $ do
                            Curses.moveCursor (fst cursor) (snd cursor + 1)
                        Curses.render
                        loop
                    Curses.EventCharacter 'h' -> do
                        cursor <- Curses.getCursor w
                        Curses.updateWindow w $ do
                            Curses.moveCursor (fst cursor) (snd cursor - 1)
                        Curses.render
                        loop
                    Curses.EventCharacter 'k' -> do
                        cursor <- Curses.getCursor w
                        Curses.updateWindow w $ do
                            Curses.moveCursor (fst cursor - 1) (snd cursor)
                        Curses.render
                        loop
                    Curses.EventCharacter 'j' -> do
                        cursor <- Curses.getCursor w
                        Curses.updateWindow w $ do
                            Curses.moveCursor (fst cursor + 1) (snd cursor)
                        Curses.render
                        loop
                    Curses.EventCharacter 'q' -> return ()
                    otherwise -> loop


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
