module MagicSquare where

import Data.Matrix

type MagicSquare = Matrix Int

newMagicSquare :: [ Int ] -> MagicSquare
newMagicSquare xs = let n = (floor . sqrt . fromIntegral) $ length xs in
  fromList n n xs

-- Computes squared diferences between sums of rows, columns and diagonals 
squaredDiferences :: MagicSquare -> Int
squaredDiferences square =
  let lines = (trace square)
            : (trace . fromLists) (map (\ row -> reverse row ) (toLists square))
            : map (\ row -> sum row ) (toLists square)
            ++ map (\ row -> sum row ) (toLists $ transpose square) 
  in
  let 
    squaredDiferences' [_] = 0
    squaredDiferences' (l1:l2:ls) = (l1 - l2)^2 
                                  + squaredDiferences' (l2:ls)
  in
  squaredDiferences' lines
