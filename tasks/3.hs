import System.Random (randomRIO)
import Data.List (permutations, nub)
import Control.Monad (forM_)

cities :: [City]
cities = [(1, 0, 0), (2, 1, 0), (3, 1, 1), (4, 0, 1)]

initializePopulation :: Int -> [City] -> IO [Tour]
initializePopulation size cities = sequence $ replicate size (randomTour cities)

randomTour :: [City] -> IO Tour
randomTour cities = do
    gen <- newStdGen
    return $ nub . take (length cities) . randoms $ gen

select :: [Tour] -> IO (Tour, Tour)
select population = do
    let fitnesses = map fitness population
    let totalFitness = sum fitnesses
    let probabilities = map (/ totalFitness) fitnesses
    index1 <- rouletteWheelSelect probabilities
    index2 <- rouletteWheelSelect probabilities
    return (population !! index1, population !! index2)

rouletteWheelSelect :: [Float] -> IO Int
rouletteWheelSelect probabilities = do
    let accumulated = scanl1 (+) probabilities
    randomValue <- randomRIO (0.0, 1.0)
    return $ length (takeWhile (< randomValue) accumulated)

crossover :: Tour -> Tour -> IO (Tour, Tour)
crossover parent1 parent2 = do
    let n = length parent1
    start <- randomRIO (0, n - 1)
    end <- randomRIO (start, n - 1)
    let offspring1 = takeSegment start end parent1 ++ filter (`notElem` takeSegment start end parent1) parent2
    let offspring2 = takeSegment start end parent2 ++ filter (`notElem` takeSegment start end parent2) parent1
    return (offspring1, offspring2)

takeSegment :: Int -> Int -> [a] -> [a]
takeSegment start end xs = take (end - start + 1) . drop start $ xs

swap :: Int -> Int -> [a] -> [a]
swap i j xs =
    let
        elemI = xs !! i
        elemJ = xs !! j
        left = take i xs
        middle = take (j - i - 1) (drop (i + 1) xs)
        right = drop (j + 1) xs
    in left ++ [elemJ] ++ middle ++ [elemI] ++ right

nextGeneration :: [Tour] -> IO [Tour]
nextGeneration population = do
    newPopulation <- replicateM (length population `div` 2) $ do
        (parent1, parent2) <- select population
        (offspring1, offspring2) <- crossover parent1 parent2
        mutatedOffspring1 <- mutate offspring1
        mutatedOffspring2 <- mutate offspring2
        return [mutatedOffspring1, mutatedOffspring2]
    return $ concat newPopulation

main :: IO ()
main = do
    let populationSize = 50
    let generations = 100
    finalPopulation <- geneticAlgorithm cities populationSize generations

    let bestTour = maximumBy (comparing fitness) finalPopulation
    putStrLn "Best tour found:"
    printTour bestTour
    putStrLn $ "Total distance: " ++ show (tourDistance bestTour)

printTour :: Tour -> IO ()
printTour tour = forM_ tour $ \(cityId, _, _) -> putStr (show cityId ++ " ")
