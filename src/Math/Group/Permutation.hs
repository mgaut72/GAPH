{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Math.Group.Permutation where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (nub)
import Data.Tuple (swap)
import Prelude hiding (lookup, negate)

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
permutation (a:as) = P $ clean $ permutation' a (a:as)
 where permutation' first [a] = M.singleton a first
       permutation' first (a:b:as) = M.insert a b $ permutation' first (b:as)
       clean = M.filterWithKey (/=)

permutationGroup :: (Ord a) => [Cycle a] -> S.Set (Permutation a)
permutationGroup = S.fromList . map permutation

maxElem :: Permutation a -> Maybe a
maxElem (P p) = if M.null p
                  then Nothing
                  else Just . fst . M.findMax $ p

-- when we look up an element, if it wasn't in the permutation explicitly,
-- we assume it was mapped to itself
lookup :: (Ord a) => a -> Permutation a -> a
lookup a (P p) = M.findWithDefault a a p

fixedBy :: (Ord a) => a -> Permutation a -> Bool
a `fixedBy` p = lookup a p == a

allFixes :: (Ord a) => S.Set (Permutation a) -> a -> Bool
allFixes ps a = any (not . fixedBy a) $ S.toList ps

compose :: (Ord a) => Permutation a -> Permutation a -> Permutation a
pa@(P a) `compose` pb@(P b) = P $ clean $ M.union x a
 where x = M.map (flip lookup pa) b
       clean = M.filterWithKey (/=)


negate :: (Ord a) => Permutation a -> Permutation a
negate (P a) = P . M.fromList . Prelude.map swap . M.toList $ a


zero :: (Ord a) => Permutation a
zero = P M.empty

-- generators for the symmetric group of order n
sym :: Int -> S.Set (Permutation Int)
sym 1 = S.singleton zero
sym 2 = S.singleton $ permutation [1,2]
sym n = S.fromList [permutation [1,2], permutation [1..n]]
