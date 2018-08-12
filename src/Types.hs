module Types where

type Id = Int
type Position = (Int, Int)
data Mode = CommandMode | InsertMode | LastLineMode | VisualMode
    deriving (Show, Eq)
