{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Math.Group.Permutation where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (find)
import Data.Maybe (isJust)
import Data.Graph.Inductive.Tree (Gr)
import qualified Data.Graph.Inductive as G
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
permutation (a:as) = P $ clean $ permutation' a (a:as)
 where permutation' first [a] = M.singleton a first
       permutation' first (a:b:as) = M.insert a b $ permutation' first (b:as)
       clean = M.filterWithKey (/=)


-- when we look up an element, if it wasn't in the permutation explicitly,
-- we assume it was mapped to itself
lookup :: (Ord a) => a -> Permutation a -> a
lookup a (P p) = M.findWithDefault a a p


compose :: (Ord a) => Permutation a -> Permutation a -> Permutation a
pa@(P a) `compose` pb@(P b) = P $ clean $ M.union x a
 where x = M.map (flip lookup pa) b
       clean = M.filterWithKey (/=)


negate :: (Ord a) => Permutation a -> Permutation a
negate (P a) = P . M.fromList . Prelude.map swap . M.toList $ a


zero :: (Ord a) => Permutation a
zero = P M.empty

--
-- Orbit Calculation
--

orbit :: (Ord a) => a -> S.Set (Permutation a) -> S.Set a
orbit g generators = S.fromList $ orbit' (S.toList generators) [g]

orbit' gens set
  | S.fromList new == S.fromList set = set
  | otherwise  = orbit' gens $ nub (set ++ new)
 where new = nub [lookup y x | x <- gens, y <- set]

--
-- Schreier Tree Construction
--
schreierTree
  :: (Enum a, Ord a) =>
       a -> S.Set (Permutation a) -> Gr a (Permutation a)
schreierTree a gens = tree gens [a] $ G.insNode (fromEnum a, a) G.empty

tree _ [] t = t
tree gens as t = tree gens as' t'
 where (as', t') = foldl (applyGens gens) ([],t) as

applyGens gens newAndTree n = S.foldl (addNode n) newAndTree gens

addNode n (ns,t) perm = if (fromEnum n') `G.gelem` t
                          then (ns,t)
                          else (ns ++ [n'], newT)
 where n' = lookup n perm
       nd n = (fromEnum n, n)
       newT = G.insEdge (fromEnum n', fromEnum n, perm) . G.insEdge (fromEnum n, fromEnum n', perm) . G.insNode (nd n') $ t

isEdge t i b ib = isJust $ find match es
 where es = G.labEdges t
       match (a,b,_) = (e i == a && e ib == b) || (e ib == a && e i == b)
       e n = fromEnum n

path t a n = foldl (compose) zero . map snd . tail $ ns
 where G.LP ns = G.lesp a n t

--
-- Stabilizer calculation
--
stabilizer a gens = foldl checkAdd S.empty l
 where l = [ (i, b, lookup i b) | b <- S.toList gens, i <- S.toList (orbit a gens)]
       checkAdd s (i,b,ib) = if isEdge t i b ib
                               then s
                               else S.insert (e i b ib) s
       e i b ib = path t a i `compose` b `compose` negate (path t a ib)
       t = schreierTree a gens

-- generators for the symmetric group of order n
sym 1 = S.singleton zero
sym 2 = S.singleton $ permutation [1,2]
sym n = S.fromList [permutation [1,2], permutation [1..n]]
