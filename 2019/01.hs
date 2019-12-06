import System.IO
import Data.Function

------------------------------------------------------------------------
-- Part 1
------------------------------------------------------------------------

calcFuel :: Integer -> Integer
calcFuel n =  n `div` 3 - 2

part1 :: String -> Integer
part1 = sum . map (calcFuel . read) . lines

------------------------------------------------------------------------
-- Part 2: Explicit recursion
------------------------------------------------------------------------

calcFuel2 :: Integer -> Integer
calcFuel2 n 
    | extra > 0 = extra + calcFuel2 extra
    | otherwise = 0
  where
    extra = n `div` 3 - 2

part2 :: String -> Integer
part2 = sum . map (calcFuel2 . read) . lines

------------------------------------------------------------------------
-- Part 2: Recursion schemes
-- TODO: Read papers. Learn more maf.
------------------------------------------------------------------------

main :: IO ()
main = do 
  input <- getContents
  print $ show $ part1 input
  print $ show $ part2 input
  