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
addPos (row1, col1) (row2, col2) = (row1 + row2, col1 + col2)

posInRange :: Position -> V4 -> Bool
posInRange (row, col) (fromRow, fromCol, toRow, toCol) = 
    row >= fromRow && row <= toRow
    &&
    col >= fromCol && col <= toCol

posDiff :: Position -> V4 -> V2
posDiff (row, col) (fromRow, fromCol, toRow, toCol) =
    (
        if row < fromRow then row - fromRow else if row > toRow then row - toRow else 0,
        if col < fromCol then col - fromCol else if col > toCol then col - toCol else 0
    )

addV2 :: V2 -> V2 -> V2
addV2 (a, b) (c, d) = (a+c, b+d)

subV2 :: V2 -> V2 -> V2
subV2 (a, b) (c, d) = (a-c, b-d)
