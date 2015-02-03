{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Math.Group where

import Data.Maybe (fromJust)

import qualified Data.Set as S
import qualified Math.Algebra.LinearAlgebra as L

import qualified Math.Group.Permutation as P
import qualified Math.Group.Matrix as M

class Group a where
  identity :: a
  inverse  :: a -> a
  times    :: a -> a -> a

instance Ord a => Group (P.Permutation a) where
  identity = P.zero
  inverse  = P.negate
  times    = P.compose

instance (Eq a, Num a, Fractional a) => Group (M.Matrix a) where
  identity = undefined -- how do I determine the size?
  inverse  = fromJust . L.inverse  -- assumes matrix was invertible
  times    = (L.<<*>>)


-- Group consisting of elements of type a acting on a set of elements of type b
class Group a => GroupAction a b where
  action :: a -> b -> b

instance Ord a => GroupAction (P.Permutation a) a where
  action = flip P.lookup

instance (Eq a, Num a, Fractional a) => GroupAction (M.Matrix a) (M.Vector a) where
  action = (L.<<*>)
