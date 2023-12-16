module Main where

import Data.List (sortBy, delete)
import Data.Function (on)

type Square = (Int, Int)
type Path = [Square]

-- Chessboard size
boardSize :: Int
boardSize = 8

-- Check if a square is on the board
onBoard :: Square -> Bool
onBoard (x, y) = x >= 1 && x <= boardSize && y >= 1 && y <= boardSize

-- Generate possible moves for a knight from a given square
knightMoves :: Square -> [Square]
knightMoves (x, y) = filter onBoard
  [(x+2, y+1), (x+2, y-1), (x-2, y+1), (x-2, y-1),
   (x+1, y+2), (x+1, y-2), (x-1, y+2), (x-1, y-2)]

-- Warnsdorff's heuristic: sort the moves based on the number of onward moves
warnsdorff :: Path -> [Square] -> [Square]
warnsdorff path moves = sortBy (compare `on` numberOfOnwardMoves) moves
  where
    numberOfOnwardMoves move = length $ validMoves (move:path)
    validMoves p = filter (`notElem` p) $ knightMoves (head p)

-- Recursive function to find the knight's tour using Warnsdorff's rule
knightTour :: Path -> [Path]
knightTour path
  | length path == boardSize * boardSize = [path]
  | otherwise = concatMap knightTour newPaths
  where
    currentSquare = head path
    possibleMoves = warnsdorff path $ knightMoves currentSquare
    newPaths = map (:path) possibleMoves

-- Find a knight's tour starting from (1, 1)
findKnightsTour :: [Path]
findKnightsTour = knightTour [(1, 1)]

-- Main function to print a solution
main :: IO ()
main = print $ head findKnightsTour

