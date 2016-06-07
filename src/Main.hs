module Main where

import Data.Maybe
import System.Environment
import System.Exit
import System.Random
import Graphics.EasyPlot
import Init
import Individual
import MagicSquare
import Population

helpMessage :: IO ()
helpMessage = do
  putStrLn "To run this program type:"
  putStrLn "geneticMagicsquares, max number of iterations, magic square size, size of the population, mutation rate, crossover rate, number of elite elements that are preserved butween generations, tournament size"
  putStrLn "geneticMagicsquares Int Int Int Float Float Int Int"
  
readn :: String -> Int
readn s = read s :: Int

main :: IO ()
main = do 
  args <- getArgs
  if head args == "--help" || null args
    then helpMessage
    else case length args of
      7 -> do
        let iter = read $ head args :: Int -- max number of generations
        let sideSize = read (args !! 1) :: Int -- magic square side's size
        let popSize = read (args !! 2) :: Int -- population size
        let mutationRate = read (args !! 3) :: Float
        let crossoverRate = read (args !! 4) :: Float
        let elite = read (args !! 5) :: Int -- elitism allows the fittest individual,
                                            -- or individuals, to be added unaltered 
                                            -- to the next generations population. 
                                            -- Size of elit population
        let tournamentSize = read (args !! 6) :: Int
  
        population' <- createPopulation popSize sideSize
        let population = calcFitness population'

        let (_,gs) = unzip $ chromosome (head $ ordPopulation population)
        let magicSquare = newMagicSquare gs
        putStrLn "Initial solution is:"
        print magicSquare
  
        fitness <- loop 0 iter elite tournamentSize mutationRate 
                        crossoverRate population


        print $ head fitness
        print $ last fitness
        printGraphic fitness

        print "End of program, aditional output in graphic Fitness.png" 

      _ -> do
        putStrLn "Invalid input, invalid number of arguments"
        putStrLn "Type --help"

loop :: Int -> Int -> Int -> Int -> Float -> Float 
     -> Population -> IO [ (Int, Int, Float, Int) ]
loop _ 0 _ _ _ _ p = do
  printSolution (head $ ordPopulation p)
  return []
loop n iter e tSize m c p = do -- curent iteration, max iterations,
                               -- elite, tournament size, mutation rate, 
                               -- crossover rate,
                               -- previous population 
  p' <- newGeneration e tSize m c p
  let f = fromMaybe (error "Fitness not available, module Main, function loop") 
                    (fitness (head $ ordPopulation p'))

  -- printPopulation p'

  let result = (n, f, calcFitnessPopulation p', length p')
  rest <- loop (n+1) (if f == 0 then 0 else iter-1) e tSize m c p' 

  return (result : rest) 

printGraphic :: [ (Int, Int, Float, Int) ] -> IO Bool
printGraphic fitness =
  plot (PNG "Fitness.png") 
    [ Data2D [ Title "Mean Population Fitness", Style Lines, Color Red ] []  
      $ map (\ (i, _, fp, _) -> (fromIntegral i, fp) ) fitness
    , Data2D [ Title "Best Individual Fitness", Style Lines, Color Blue ] []
      $ map (\ (i, f, _, _) -> (fromIntegral i, fromIntegral f) ) fitness
    ]

printSolution :: Individual -> IO ()
printSolution i = do
  let (_,gs) = unzip $ chromosome i
  let magicSquare = newMagicSquare gs
  putStrLn "--------------------"
  putStrLn ("Total fitness: " ++ show (fitness i))
  putStrLn "--------------------"
  putStrLn "Best solution is:"
  print magicSquare
  putStrLn "--------------------"

printPopulation :: Population -> IO ()
printPopulation p = do
  let squares = map (\ i -> let (_,gs) = unzip $ chromosome i in  
                             newMagicSquare gs ) $ take 4 (ordPopulation p)
  putStrLn "--------------------"
  mapM print squares
  putStrLn "--------------------"
