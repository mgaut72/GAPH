module Math.Group.Matrix where

import qualified Data.Set as S
import Data.List
import Control.Lens

import Math.Algebra.LinearAlgebra

type Matrix a = [[a]]
type Vector a = [a]

--orbit :: S.Set (Matrix a) -> Vector a -> [Vector a]
orbit ms v = S.fromList $ orbit' (S.toList ms) [v]

orbit' [] ls = ls
orbit' (m:ms) ls = orbit' ms (checkMatrix m ls)

checkMatrix m ls = if null notInSpan then ls else checkMatrix m newLs
 where lsM = map lM ls
       els = rowEchelonForm ls
       notInSpan = filter (not . inSpanRE els) lsM
       lM l = l <*>> m
       newLs = nub $ ls ++ notInSpan

--addToOrbit :: Fractional a => Vector a -> ([Vector a], [Vector a]) -> ([Vector a], [Vector a])
addToOrbit v (ls, els) = (ls ++ [v], rowEchelonForm $ els ++ [v])

vec size x = replicate size 0.0 & (element (x-1) .~ 1.0 )

v = [0.0,0.0,0.0,0.0,1.0]

ms = S.fromList [m1,m2]

m1 = [ [0.0,0.0,1.0,0.0,0.0]
     , [1.0,0.0,0.0,0.0,0.0]
     , [0.0,1.0,0.0,0.0,0.0]
     , [0.0,0.0,0.0,1.0,0.0]
     , [0.0,0.0,0.0,0.0,1.0]]

m2 = [ [1.0,0.0,0.0,0.0,0.0]
     , [0.0,0.0,0.0,1.0,0.0]
     , [0.0,1.0,0.0,0.0,0.0]
     , [0.0,0.0,1.0,0.0,0.0]
     , [0.0,0.0,0.0,0.0,1.0]]
