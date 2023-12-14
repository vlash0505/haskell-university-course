import Data.List (delete)

type Square = (Int, Int)
type Path = [Square]

boardSize :: Int
boardSize = 8

onBoard :: Square -> Bool
onBoard (x, y) = x >= 1 && x <= boardSize && y >= 1 && y <= boardSize

knightMoves :: Square -> [Square]
knightMoves (x, y) = filter onBoard
  [(x+2, y+1), (x+2, y-1), (x-2, y+1), (x-2, y-1),
   (x+1, y+2), (x+1, y-2), (x-1, y+2), (x-1, y-2)]

knightTour :: Path -> [Path]
knightTour path
  | length path == boardSize * boardSize = [path]
  | otherwise = concatMap knightTour newPaths
  where
    currentSquare = head path
    possibleMoves = knightMoves currentSquare
    validMoves = filter (`notElem` path) possibleMoves
    newPaths = map (:path) validMoves

findKnightsTour :: [Path]
findKnightsTour = knightTour [(1, 1)]

main :: IO ()
main = print $ head findKnightsTour
