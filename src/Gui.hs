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

getWindowSize :: CWindow -> Curses.Curses V2
getWindowSize w = Curses.updateWindow w $ do
    (height, width) <- Curses.windowSize
    return (fromInteger height, fromInteger width)

prepLine :: Int -> Int -> Text.Text -> Text.Text
prepLine from width line = Text.take (fromIntegral width) $ Text.drop (fromIntegral from) line

drawTextAt :: Int -> Int -> Size -> Text.Text -> Curses.Update ()
drawTextAt row col (windowHeight, windowWidth) text = do
    if (row >= 0 && row < windowHeight && col >= 0 && col < windowWidth) then do
        Curses.moveCursor (toInteger row) (toInteger col)
        Curses.drawText text
        return ()
    else
        return ()

resetCursor :: Window.Window -> Curses.Update ()
resetCursor window = Curses.moveCursor (toInteger row) (toInteger col)
    where
        cursorPos = Window.getRelativeCursorPos window
        col = snd cursorPos
        row = fst cursorPos

renderAll :: State.State -> Curses.Curses ()
renderAll state = do
    case (State.getActiveWindow state) of
        Nothing -> return ()
        Just window -> do
            let cw = Window.cWindow window
            let buffer = State.getWindowBuffer state window
            let width = snd $ Window.size window
            let height = fst $ Window.size window 
            let scrollCol  = snd $ Window.scrollPos window
            let scrollRow  = fst $ Window.scrollPos window
            Curses.updateWindow cw $ do
                -- Curses.clear
                case buffer of
                    Just b -> do
                        let lines = fmap (prepLine scrollCol width) $ Sequence.drop (fromIntegral scrollRow) (Buffer.bLines b)
                        let count = (fromIntegral $ Sequence.length lines) - 1
                        mapM (\y -> drawTextAt y 0 (Window.size window) (Sequence.index lines (fromIntegral y))) [0..count]
                drawTextAt (height) 0 (width, height) (Text.pack (show (State.mode state )))
                resetCursor window
            return ()
