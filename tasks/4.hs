import qualified Data.Map as Map
import Data.Map (Map)

type Graph = Map Vertex [Vertex]
type Vertex = Int
type Path = [Vertex]

-- Depth-First Search function to find all paths
findAllPaths :: Graph -> Vertex -> Vertex -> [Path]
findAllPaths graph start end = dfs [start] [] start
  where
    dfs path paths current
      | current == end = [reverse path] ++ paths
      | otherwise = foldl (\acc next -> if next `elem` path then acc else dfs (next:path) acc next) paths (graph Map.! current)

main :: IO ()
main = do
  let graph = Map.fromList [(1, [2, 3]), (2, [3]), (3, [4]), (4, [])]

  print $ findAllPaths graph 1 4
