module Cell where

import Player (Player (X, O))

-- You may wish to define your own Show instance
data Cell = Empty | Taken Player deriving (Eq)

-- added Show instance for Cell
instance Show Cell where
  show Empty = "-"
  show (Taken X) = "X"
  show (Taken O) = "O"
  
-- | Returns True if the given cell is Empty
isEmpty :: Cell -> Bool
isEmpty Empty = True
isEmpty _     = False
