module Types where

import qualified UI.NCurses as Curses

type Id = Int
type Position = (Int, Int)
data Mode = CommandMode | InsertMode | LastLineMode | VisualMode
    deriving (Show, Eq)
type WindowRef = Curses.Window
