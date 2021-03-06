module Population where

import Control.Concurrent.Async
import Control.Parallel.Strategies
import Data.List
import Data.Maybe
import System.Random
import MagicSquare
import Individual
import Lib

type Population = [ Individual ]

calcFitness :: Population -> Population
calcFitness p = map (\ i -> let (_,gs) = unzip $ chromosome i in
                            i { fitness = Just (squaredDiferences $ newMagicSquare gs) } ) p `using` parList rseq

calcFitnessPopulation :: Population -> Float
calcFitnessPopulation p = 
  fromIntegral (sum $ map fit p) / fromIntegral (length p)
    where
      fit x = fromMaybe (error "Error in fitness, module Population, function calcFitnessPopulation") 
                        (fitness x)

ordPopulation :: Population -> Population
ordPopulation = sort

tournamentSelection :: Int -> Population -> IO Individual
tournamentSelection n p = do -- tournament size, previous population
  -- Tournament selection selects its parents by running a series of "tournaments".
  -- First, individuals are randomly selected from the population and entered into a
  -- tournament. Next, these individuals can be thought to compete with each other
  -- by comparing their fitness values, then choosing the individual with the highest
  -- fitness for the parent.
  is <- randList n p
  return (head $ ordPopulation is)

selectParents :: Int -> Int -> Population -> IO [ (Individual, Individual) ]
selectParents 0 _ _ = return []
selectParents n tSize p = do -- number of children, tournament size, previous population
  parent1 <- tournamentSelection tSize p
  parent2 <- tournamentSelection tSize p
  let parents = (parent1, parent2)
  rest <- selectParents (n-1) tSize p

  return (parents : rest)

crossover :: Float -> (Individual, Individual) -> IO Individual
crossover c parents = do -- number of children, crossover rate, (first parent, second parent)
  -- With the traveling salesman problem, both the genes and the order of the genes
  -- in the chromosome are very important. In fact, for the traveling salesman 
  -- problem we shouldn't ever have more than one copy of a specific gene in our
  -- chromosome. This is because it would create an invalid solution because a city
  -- shouldn't be visited more than once on a given route. Consider a case where we
  -- ve three cities: City A, City B and City C. A route of A,B,C is valid; however,
  -- a route of C,B,C is not: this route visits City C twice, and also never visits
  -- City A. Because of this, it's essential that we find and apply a crossover 
  -- method that produces valid results for our problem.
  -- We also need to be respectful of the ordering of the parent's chromosomes
  -- during the crossover process. This is because the order of the chromosome
  -- the solution's fitness. In fact, it’s only the order that matters.
  -- Here we will aply ordered crossover.
  gen <- newStdGen
  let r = head $ take 1 $ randoms gen :: Float
  if c < r 
    then return (fst parents)
    else do
      gen' <- newStdGen
      let rs = take 2 $ randomRs (0, length (chromosome $ fst parents)) gen' :: [ Int ]
      let pos1 = minimum rs
      let pos2 = maximum rs

      let ( _, chromosomeFstParent ) = unzip (chromosome $ fst parents) 
      let ( _, chromosomeSndParent ) = unzip (chromosome $ snd parents) 

      -- fst parent contribution
      let fstParentContrib = drop pos1 $ take pos2 chromosomeFstParent
      let sndParentContrib = [ x | x <- chromosomeSndParent, notElem x fstParentContrib ]
      let childChromosome =  take pos1 sndParentContrib
                          ++ fstParentContrib
		          ++ drop pos1 sndParentContrib

      let child = newIndividual (zip [ 0.. ] childChromosome)

      if length (chromosome $ fst parents) /= length (chromosome child)
        then error "Length mismatch, module Population, function crossover. \n"
        else return child

offspring :: [ (Individual, Individual) ] -> Float -> Float -> IO Population
offspring parents m c = do -- list of parents, mutation rate, crossover rate
  children <- mapConcurrently (\ ps -> crossover c ps >>= mutation m ) parents
  return children

mutation :: Float -> Individual -> IO Individual
mutation m i = do -- mutation rate, individual
  -- Swap mutation, is an algorithm that will simply swap the genetic information at
  -- two points. Swap mutation works by looping though the genes in the individual’s
  -- chromosome with each gene being considered for mutation determined by the 
  -- mutation rate. If a gene is selected for mutation, another random gene in the
  -- chromosome is picked and then their positions are swapped.

  -- Swap genes according to mutation rate (pos1 with pos2)
  let swapGene i pos1 = case pos1 of
        (-1) -> return i -- chromosome totally scanned
        otehrwise -> do
          gen <- newStdGen
          let r = head $ drop (snd $ chromosome i !! pos1) $ randoms gen :: Float
          if m < r
            then swapGene i (pos1-1)
            else do
              gen' <- newStdGen
              let pos2 = head $ drop (snd $ chromosome i !! pos1) 
                       $ randomRs (0, length (chromosome i) - 1) gen' :: Int
              let g1 = (pos1, snd $ chromosome i !! pos2)
              let g2 = (pos2, snd $ chromosome i !! pos1)
              let i' = modifyChromosome (modifyChromosome i g2) g1 -- swaping genes 
              swapGene i' (pos1-1)
  swapGene i (length (chromosome i) - 1)

newGeneration :: Int -> Int -> Float -> Float
              -> Population -> IO Population
newGeneration e tSize m c p = do -- elite, tournament size, mutation rate,
                                 -- crossover rate, previous population
    let pElite = take e $ ordPopulation p 
    parents <- selectParents (length p - e) tSize p
    p' <- offspring parents m c
    return (calcFitness $ pElite ++ p')
