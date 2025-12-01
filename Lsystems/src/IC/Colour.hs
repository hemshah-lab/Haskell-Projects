module IC.Colour where

data Colour = Black | White
            | Red | Green | Blue
            | Cyan | Magenta | Yellow
            | Custom Float Float Float
            deriving (Show, Eq, Ord)
