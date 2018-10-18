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

prepLine :: Integer -> Text.Text -> Text.Text
prepLine width line = Text.take (fromIntegral width) line

drawTextAt :: Integer -> Integer -> Text.Text -> Curses.Update ()
drawTextAt y x text = do
    Curses.moveCursor y x
    Curses.drawText text

renderAll :: State.State -> Curses.Curses ()
renderAll state = do
    case (State.getActiveWindow state) of
        Nothing -> return ()
        Just window -> do
            let cw = Window.cWindow window
            let buffer = State.getWindowBuffer state window
            let width = snd $ State.screenSize state
            Curses.updateWindow cw $ do
                case buffer of
                    Just b -> do
                        let lines = fmap (prepLine width) $ Buffer.bLines b
                        let count = (fromIntegral $ Sequence.length lines) - 1
                        mapM (\y -> drawTextAt y 0 (Sequence.index lines (fromIntegral y))) [0..count]
            return ()
