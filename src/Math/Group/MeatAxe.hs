module Math.Group.MeatAxe where

import Math.Algebra.LinearAlgebra
import System.Random
import qualified Data.Set as S

import qualified Math.Group as G
import Math.Group.Matrix


--
-- MeatAxe
-- m is a group described by the matrices
--

-- given a matrixgroup M, costruct a random word from elements of the group
-- for simplicity, we choose between 1 and 5 random matrices from the group
-- and multiply them.  A proper random word should be sort of a linear
-- combination with random weights of a random number of random elements from
-- the set of matrics, but this is not implemented here
randWordIn :: (Eq a, Num a, Fractional a) => S.Set (Matrix a) -> IO (Matrix a)
randWordIn ms = do
  g <- newStdGen
  let (len, g') = randomR (1,5) g
  let randomElems = map (flip S.elemAt ms) . take len $ randomRs (0, S.size ms) g'
  return $ foldl1 G.times randomElems

-- helper function to not trap everything in IO
-- fs is a list of all elements in the Feild our matrices are over
-- ms is the set of matices that describe our group
meataxe fs ms = do
  w <- randWordIn ms
  let basisNullW = kernel w
  if null (tail basisNullW) && isZero (head basisNullW) -- w was singular
    meataxe fs ms -- try again
    meataxe' fs ms w basisNullW -- we're good with this W, resume the algorith



-- function to iterate over all possible weights for a linear combination
-- of n elements, given a list of all elements in the feild we are working in
weights _ 0   = []
weights lst 1 = map (:[]) lst
weights lst n = concatMap (\l -> map (l:) (combos lst (n-1))) lst

linearCombinations ws vs = map (linearCombo vs) wss
 where wss = weights ws (length vs)

linearCombo vs ws = foldl1 (<+>) $ zipWith (*>) ws vs
