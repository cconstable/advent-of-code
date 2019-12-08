import Data.Char
import Data.List.Split
import Data.List
import Data.Ord
import System.IO

imageWidth = 25
imageHeight = 6
imageSize = imageWidth * imageHeight

------------------------------------------------------------------------
-- Part 1
------------------------------------------------------------------------

numOf :: Eq a => a -> [a] -> Int
numOf a = length . filter (==a)

part1 :: [Int] -> Int
part1 = product1sAnd2s . leastZeroSeq
 where
    leastZeroSeq = minimumBy (comparing (numOf 0)) . chunksOf imageSize  
    product1sAnd2s xs = numOf 1 xs * numOf 2 xs

------------------------------------------------------------------------
-- Part 2
------------------------------------------------------------------------

addPixel :: Integral a => a -> a -> a
addPixel 2 b = b
addPixel a _ = a 

part2 :: [Int] -> [Int]
part2 = foldl1 (zipWith addPixel) . chunksOf imageSize 

printImage :: [Int] -> IO ()
printImage = putStr . unlines . map digitsToString . chunksOf imageWidth
  where
    digitsToString = concat . (map digitToChar)
    digitToChar 0 = "   "
    digitToChar 1 = " # "

main :: IO ()
main = do 
  input <- getContents
  let digits = map digitToInt input
  print $ show $ part1 digits
  printImage $ part2 digits
  