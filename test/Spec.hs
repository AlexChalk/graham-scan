import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord
import Lib

testDatapoints :: [Point]
testDatapoints = map toPoint 
  [
    (2, 6),
    (4, 6),
    (3, 5),
    (0.1, 4),
    (2.1, 4),
    (4.1, 4),
    (6.1, 4),
    (1.3, 3),
    (3.3, 3),
    (5.3, 3),
    (0, 2),
    (2.1, 2),
    (4.1, 2),
    (6.1, 2),
    (3, 1),
    (2, 0),
    (4, 0) 
  ]

expectedDatapoints :: [Point]
expectedDatapoints = map toPoint 
  [
    (0, 2),
    (0.1, 4),
    (2, 6),
    (4, 6),
    (6.1, 4),
    (6.1, 2),
    (4, 0),
    (2, 0)
  ]

main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [unitTests]

unitTests = testGroup "unit tests"
  [ 
    testCase "datapoints" $ 
      assertEqual "grahamScan" expectedDatapoints (grahamScan testDatapoints)
  ]
