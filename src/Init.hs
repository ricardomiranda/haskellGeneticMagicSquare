module Init where

import System.Random
import Lib
import Individual
import Population

createPopulation :: Int -> Int -> IO Population
createPopulation 0 _ = return []
createPopulation popSize sideSize = do -- population size, magic square's side size
  individual <- createIndividual sideSize 
  rest <- createPopulation (popSize-1) sideSize
  return (individual : rest)
