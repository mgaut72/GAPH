module Math.Group.Orbit (
  orbit
) where

import Math.Group
import qualified Data.Set as S
import Data.List (nub)

orbit generators g = S.fromList $ orbit' (S.toList generators) [g]

orbit' gens set
  | S.fromList new == S.fromList set = set
  | otherwise  = orbit' gens $ nub (set ++ new)
 where new = nub [action x y | x <- gens, y <- set]
