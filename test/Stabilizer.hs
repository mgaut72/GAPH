import System.Exit
import Test.HUnit

import qualified Data.Set as S

import qualified Math.Group.Permutation as P

import Math.Group.Stabilizer

main = do
  cs <- runTestTT tests
  if failures cs /= 0 || errors cs /= 0
     then exitFailure
     else exitSuccess

tests = TestList [ TestLabel "s1" s1
--                 , TestLabel "s2" s2
--                 , TestLabel "s3" s3
                 ]

s1 = stabilizer g a ~?= P.permutationGroup [[2,3]]
 where g = P.sym 3 :: S.Set (P.Permutation Int)
       a = 1 :: Int
