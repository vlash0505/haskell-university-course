module Main where

import Control.Concurrent
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import System.Clock
import Data.List (minimumBy, delete)
import Data.Ord (comparing)

type Node = Int
type Weight = Int
type Edge = (Node, Weight)
type Graph = Map Node [Edge]

dijkstra :: Graph -> Node -> Map Node Weight
dijkstra graph start = dijkstra' initialDistances [start]
  where
    initialDistances = Map.fromList [(node, if node == start then 0 else maxBound) | node <- Map.keys graph]

    dijkstra' distances unvisited
      | null unvisited = distances
      | otherwise =
          let filteredDistances = [(node, dist) | (node, dist) <- Map.toList distances, node `elem` unvisited]
              (nearest, nearestDist) = minimumBy (comparing snd) filteredDistances
          in dijkstra' (updateDistances nearest nearestDist distances) (delete nearest unvisited)

    updateDistances node dist distances = foldl (updateDistance node dist) distances (Map.findWithDefault [] node graph)

    updateDistance _ srcDist distances (destNode, weight) =
      let alt = srcDist + weight in
      Map.adjust (min alt) destNode distances

-- Split the graph into subgraphs and process them in parallel
parallelDijkstra :: Graph -> [Node] -> IO [Map Node Weight]
parallelDijkstra graph starts = do
    mvars <- forM starts $ \start -> do
        mvar <- newEmptyMVar
        _ <- forkIO $ do
            let minPath = dijkstra graph start
            putMVar mvar minPath
        return mvar
    mapM takeMVar mvars

main :: IO ()
main = do
    let graph = Map.fromList [(1, [(2, 10), (3, 5)]), (2, [(4, 1)]), (3, [(4, 2)]), (4, [])]
    startTime <- getTime Monotonic
    -- Run Dijkstra's algorithm in parallel on multiple start nodes
    results <- parallelDijkstra graph [1, 2, 3]
    endTime <- getTime Monotonic
    let elapsedTime = fromIntegral (toNanoSecs (endTime - startTime)) / 1e9 :: Double

    mapM_ print results
    putStrLn $ "Elapsed time: " ++ show elapsedTime ++ " seconds"
