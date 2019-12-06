import System.IO
import Data.List

------------------------------------------------------------------------
-- Part 1
------------------------------------------------------------------------

digits :: Integral n => n -> [n]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

isIncreasing :: Ord n => [n] -> Bool
isIncreasing []     = True
isIncreasing [_]    = True
isIncreasing (x:xs) = (x <= head xs) && isIncreasing(xs)

adjDups :: Ord n => [n] -> Bool
adjDups = any ((>=2) . length) . group

part1 :: [Integer]
part1 = filter (conditions) [206938..679128]
  where
    conditions = and . sequence [isIncreasing . digits, adjDups . digits]

------------------------------------------------------------------------
-- Part 2
------------------------------------------------------------------------

atLeastOneDouble :: Ord a => [a] -> Bool
atLeastOneDouble = any ((==2) . length) . group

part2 :: [Integer]
part2 = filter (conditions) [206938..679128]
  where
    conditions = and . sequence [isIncreasing . digits, atLeastOneDouble . digits]

main :: IO ()
main = do 
    print $ length part1
    print $ length part2
