module Main where

import TicTacToe
import Board
import Cell
import Player
import Helpers

import Text.Read (readMaybe)
import Data.List (intersperse)

import System.IO (hSetBuffering, stdout, stdin, BufferMode(NoBuffering))

-- substitutions for printing
prettyPrint :: Board -> IO ()
prettyPrint board = do
  putStrLn (unlines (map (unwords . map show) (rows board)))

-- | Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn board player = do
  prettyPrint board
  putStrLn ("Player " ++ show player ++ ", enter your move: ")
  position <- getLine
  case parsePosition position of
    Nothing -> do
      putStrLn "Invalid move, try again."
      takeTurn board player
    Just pos -> case tryMove player pos board of
      Nothing -> do
        putStrLn "Invalid move, try again."
        takeTurn board player
      Just newBoard -> do
        return newBoard


-- | Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame board player = do
  if gameOver board
    then do 
      prettyPrint board
      putStrLn ("Player " ++ show (swap player) ++ " wins!")
  -- this check has to go second as the game can be over when the board full
  else if isBoardFull board
    then do
      prettyPrint board
      putStrLn "Draw!"
  else do
    newBoard <- takeTurn board player
    playGame newBoard (swap player)
  
-- | Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main = do
  putStrLn "Welcome to TicTacToe! Choose the board dimension (nxn):"
  n <- getLine
  putStrLn "Player X Starts!"
  playGame (emptyBoard (read n)) X
  putStrLn "Thanks for playing!"
  disableBuffering
  return ()

{-|
When ran via `cabal run`, Haskell will "buffer" the input and output streams
for performance. This is annoying, since it means that some printing can happen
out-of-order -- the strings are only written when a newline is entered!

There are a few ways of getting around this, like using `hFlush stdout` after
each print. However, in this case, it is simplest to just disable the buffering
altogether. This function *must* be called at the start of your `main` function.
-}
disableBuffering :: IO ()
disableBuffering = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
