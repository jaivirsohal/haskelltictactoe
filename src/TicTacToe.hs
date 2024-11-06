module TicTacToe (gameOver, parsePosition, tryMove) where

import Data.List (nub)
import Text.Read (readMaybe)

import Board
import Cell
import Player
import Helpers

type Position = (Int, Int)

gameOver :: Board -> Bool
gameOver board@(Board n grid) = diagWin || rowWin || colWin
    where diagWin = checkWin (diags board) n
          rowWin = checkWin (rows board) n
          colWin = checkWin (cols board) n
          checkWin :: [[Cell]] -> Int -> Bool
          checkWin xss n = length [xs | xs <- xss, Empty `elem` xs] /= n

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition xs
    | (readMaybe first :: Maybe Int) == Nothing = Nothing
    | (readMaybe second :: Maybe Int) == Nothing = Nothing
    | otherwise = Just (read first, read second)
    where (first, second) = split xs
          split :: String -> (String, String)
          split "" = ("", "")
          split (x:xs)
            | [x] == " " = ("", xs)
            | otherwise = let (first, second) = split xs
                          in  (x:first, second)

tryMove :: Player -> Position -> Board -> Maybe Board
tryMove player pos@(i,j) board@(Board n grid)
    | valid = Just newBoard
    | otherwise = Nothing
    where index = i * n + j
          withinIndex = (index >= 0) && (index <= length grid - 1)
          valid = withinIndex && isEmpty (grid!!index)
          newBoard = Board n (replace index (Taken player) grid)