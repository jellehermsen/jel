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

module Helpers where

import Types


addPos :: Position -> Position -> Position
addPos (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

posX :: Position -> Int
posX (_, x) = x

posY :: Position -> Int
posY (y, _) = y

posInRange :: Position -> V4 -> Bool
posInRange (y, x) (fromY, fromX, toY, toX) = 
    y >= fromY && y <= toY
    &&
    x >= fromX && x <= toX

posDiff :: Position -> V4 -> V2
posDiff (y, x) (fromY, fromX, toY, toX) =
    (
        if y < fromY then y - fromY else if y > toY then y - toY else 0,
        if x < fromX then x - fromX else if x > toX then x - toX else 0
    )

addV2 :: V2 -> V2 -> V2
addV2 (a, b) (c, d) = (a+c, b+d)

subV2 :: V2 -> V2 -> V2
subV2 (a, b) (c, d) = (a-c, b-d)
