{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Math.Group.Permutation where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (nub)
import Prelude hiding (lookup, negate)
import Data.Tuple (swap)

-- A permutation is a Map, since indexing into a list is slow in haskell
newtype Permutation a = P (M.Map a a) deriving (Ord)
type Cycle a = [a]

instance (Ord a) => Eq (Permutation a) where
  p1 == p2 = p1 `myeq` p2 && p2 `myeq` p1
   where a@(P a') `myeq` b = all (\k -> lookup k a == lookup k b) (M.keys a')

instance (Show a) => Show (Permutation a) where
  show (P a) = show a


-- takes a list, that is a cycle
-- generates a permutation based on that cycle
permutation :: (Ord a) => Cycle a -> Permutation a
permutation [] = zero
permutation [a] = zero
permutation (a:as) = P $ permutation' a (a:as)
 where permutation' first [a] = M.singleton a first
       permutation' first (a:b:as) = M.insert a b $ permutation' first (b:as)


-- when we look up an element, if it wasn't in the permutation explicitly,
-- we assume it was mapped to itself
lookup :: (Ord a) => a -> Permutation a -> a
lookup a (P p) = M.findWithDefault a a p


compose :: (Ord a) => Permutation a -> Permutation a -> Permutation a
pa@(P a) `compose` pb@(P b) = P $ M.union x a
 where x = M.map (flip lookup pa) b


negate :: (Ord a) => Permutation a -> Permutation a
negate (P a) = P . M.fromList . Prelude.map swap . M.toList $ a


zero :: (Ord a) => Permutation a
zero = P M.empty

orbit :: (Ord a) => Permutation a -> S.Set (Permutation a) -> S.Set (Permutation a)
orbit g generators = S.fromList $ orbit' (S.toList generators) [g]

orbit' gens set
  | S.fromList new == S.fromList set = set
  | otherwise  = orbit' gens $ nub (set ++ new)
 where new = step gens set

step gens set = nub [y `compose` x | x <- gens, y <- set]

-- generators for the symmetric group of order n
sym 1 = S.singleton zero
sym 2 = S.singleton $ permutation [1,2]
sym n = S.fromList [permutation [1,2], permutation [1..n]]
