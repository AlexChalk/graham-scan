module Lib where

import Data.List
import Data.Aeson

data Point = Point Double Double
  deriving (Show, Eq)

toPoint :: (Double, Double) -> Point
toPoint (a, b) = Point a b

data Direction = Straight 
               | TurnLeft 
               | TurnRight 
               deriving (Show)


grahamScan :: [Point] -> [Point]
grahamScan points
  | length points < 3 = points
  | otherwise = scan [lowest_point] ordered_rest
  where
    ordered_by_depth = sortBy lowestPoint points
    lowest_point:rest = ordered_by_depth
    ordered_rest = sortBy (compareAngleWithXAxisFrom lowest_point) rest


lowestPoint :: Point -> Point -> Ordering
lowestPoint (Point x1 y1) (Point x2 y2)
  | y1 < y2 = LT
  | y1 > y2 = GT
  | y1 == y2 = compare x1 x2

compareAngleWithXAxisFrom :: Point -> Point -> Point -> Ordering
compareAngleWithXAxisFrom point_of_comparison a b =
  case getDirection point_of_comparison a b of
    Straight -> EQ
    TurnLeft -> LT
    TurnRight -> GT

crossProduct :: Point -> Point -> Point -> Double
crossProduct (Point x1 y1) (Point x2 y2) (Point x3 y3) =
  (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1) 


-- https://wiki.haskell.org/Graham_Scan_Implementation
-- Unfortunately I was not smart enough to figure out 'scan' 
-- myself this time around.
scan :: [Point] -> [Point] -> [Point]
scan (x:xs) (y:z:rest) = case getDirection x y z of
  TurnRight -> scan xs (x:z:rest)
  Straight -> scan (x:xs) (z:rest)
  TurnLeft -> scan (y:x:xs) (z:rest)

scan xs [z] = z : xs


getDirection :: Point -> Point -> Point -> Direction
getDirection a b c =
  case signum (crossProduct a b c) of
    0 -> Straight
    1 -> TurnLeft
    -1 -> TurnRight
