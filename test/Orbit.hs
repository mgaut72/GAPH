import System.Exit
import Test.HUnit

import qualified Data.Set as S

import Math.Group.Orbit
import qualified Math.Group.Permutation as P
import qualified Math.Group.Matrix as M

main = do
  cs <- runTestTT tests
  if failures cs /= 0 || errors cs /= 0
     then exitFailure
     else exitSuccess

tests = TestList [ TestLabel "permutationOrbit1" o1
                 , TestLabel "permutationOrbit2" o2
                 , TestLabel "permutationOrbit3" o3
                 , TestLabel "matrixOrbit1" o4
                 , TestLabel "matrixOrbit2" o5
                 , TestLabel "matrixOrbit3" o6
                 ]

--
-- permutation group
--

o1 = orbit g a ~?= S.fromList [1,2,3,4]
 where g = P.permutationGroup [[1,3,2],[2,4,3]] :: S.Set (P.Permutation Int)
       a = 1 :: Int

o2 = orbit g a ~?= S.fromList [5]
 where g = P.permutationGroup [[1,3,2],[2,4,3]] :: S.Set (P.Permutation Int)
       a = 5 :: Int

o3 = orbit g a ~?= S.fromList [1,2,3,4,5,8]
 where g = P.permutationGroup [[1,3,2],[2,4,3], [2,5,8]] :: S.Set (P.Permutation Int)
       a = 3 :: Int


--
-- matrix group test
--

o4 = M.orbit ms v ~?= S.fromList m2
 where v = [1.0,0.0,0.0,0.0]
       ms = S.fromList [m1,m2]
       m1 = [ [0.0,0.0,1.0,0.0]
            , [1.0,0.0,0.0,0.0]
            , [0.0,1.0,0.0,0.0]
            , [0.0,0.0,0.0,1.0]]
       m2 = [ [1.0,0.0,0.0,0.0]
            , [0.0,0.0,0.0,1.0]
            , [0.0,1.0,0.0,0.0]
            , [0.0,0.0,1.0,0.0]]

o5 = M.orbit ms v ~?= S.singleton v
 where v = [0.0,0.0,0.0,0.0,1.0]
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

o6 = M.orbit ms v ~?= S.fromList (map (M.vec 8) [1,2,3,4,5,8])
 where v = M.vec 8 3
       ms = S.fromList [m1,m2,m3]
       m1 = [ M.vec 8 3
            , M.vec 8 1
            , M.vec 8 2
            , M.vec 8 4
            ]
       m2 = [ M.vec 8 1
            , M.vec 8 4
            , M.vec 8 2
            , M.vec 8 3
            ]
       m3 = [ M.vec 8 1
            , M.vec 8 5
            , M.vec 8 3
            , M.vec 8 4
            , M.vec 8 8
            , M.vec 8 6
            , M.vec 8 7
            , M.vec 8 2
            ]
