module Individual where

import Data.Sequence
import Data.Foldable
import System.Random
import System.Random.Shuffle
import MagicSquare

-- A chromosome is a sequense of numbers to solve the magic square.
-- If we have a 3x3 square the gene will have a length of 9,
-- the first 3 numbers is the top row, etc.
data Individual = Individual { chromosome :: [ (Int, Int) ] -- [ (pos, gene) ]
                             , fitness :: Maybe Int } deriving Show 

instance Eq Individual where
  Individual { fitness = a } == Individual { fitness = b } = Just a == Just b

instance Ord Individual where
  compare Individual { fitness = a } Individual { fitness = b } = compare (Just a) (Just b)

newIndividual :: [ (Int, Int) ] -> Individual
newIndividual gs = Individual { chromosome = gs
                              , fitness = Nothing }

createIndividual :: Int -> IO Individual
createIndividual n = do -- Side of magic square
  gen <- newStdGen
  let xs' = [ 1..(n*n) ]
  let xs = Prelude.zip [ 0.. ] (shuffle' xs' (Prelude.length xs') gen)
  return (newIndividual xs)

modifyChromosome :: Individual -> (Int, Int) -> Individual
modifyChromosome i g =
  i { chromosome = toList $ update (fst g) g (fromList $ chromosome i) }
