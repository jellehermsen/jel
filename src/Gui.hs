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
    along with Jel.  If not, see <https://www.gnu.org/licenses/>.
-}

module Gui where

import qualified UI.NCurses as Curses
import qualified Data.Text as Text
import qualified Data.Sequence as Sequence
import Data.List

import Types
import qualified Buffer
import qualified State
import qualified Window

getWindowSize :: CWindow -> Curses.Curses (Integer, Integer)
getWindowSize w = Curses.updateWindow w $ do
    size <- Curses.windowSize
    return size

prepLine :: Integer -> Integer -> Text.Text -> Text.Text
prepLine from width line = Text.take (fromIntegral width) $ Text.drop (fromIntegral from) line

drawTextAt :: Integer -> Integer -> Size -> Text.Text -> Curses.Update ()
drawTextAt y x (windowHeight, windowWidth) text = do
    if (y >= 0 && y < windowHeight && x >= 0 && x < windowWidth) then do
        Curses.moveCursor y x
        Curses.drawText text
        return ()
    else
        return ()

resetCursor :: Window.Window -> Curses.Update ()
resetCursor window = Curses.moveCursor y x
    where
        cursorPos = Window.getRelativeCursorPos window
        x = snd cursorPos
        y = fst cursorPos

renderAll :: State.State -> Curses.Curses ()
renderAll state = do
    case (State.getActiveWindow state) of
        Nothing -> return ()
        Just window -> do
            let cw = Window.cWindow window
            let buffer = State.getWindowBuffer state window
            let width = snd $ Window.size window
            let height = fst $ Window.size window 
            let scrollX  = snd $ Window.scrollPos window
            let scrollY  = fst $ Window.scrollPos window
            Curses.updateWindow cw $ do
                Curses.clear
                case buffer of
                    Just b -> do
                        let lines = fmap (prepLine scrollX width) $ Sequence.drop (fromIntegral scrollY) (Buffer.bLines b)
                        let count = (fromIntegral $ Sequence.length lines) - 1
                        mapM (\y -> drawTextAt y 0 (Window.size window) (Sequence.index lines (fromIntegral y))) [0..count]
                drawTextAt (height - 10) 0 (width, height) (Text.pack (show (Window.scrollPos window )))
                resetCursor window
            return ()
