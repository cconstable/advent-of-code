
import Data.Char (digitToInt)

patternForRow :: Int -> [Int]
patternForRow n = tail $ concatMap (take (fromIntegral $ n+1) . repeat) base
  where
    base = cycle [0, 1, 0, -1]

digitsToString :: [Int] -> String
digitsToString [] = "0"
digitsToString (x:[]) = show x
digitsToString (x:xs) = show x ++ digitsToString xs

-- Part 1

processFFT :: String -> String
processFFT n = digitsToString [processFFT' n i | i <- [0..len]]
  where
    processFFT' n i = (abs $ sum $ (zipWith (*) (nums) (patternForRow i))) `mod` 10
    nums = map digitToInt n
    len = (length nums) - 1

phases :: String -> [String]
phases = iterate processFFT

-- input = "59708072843556858145230522180223745694544745622336045476506437914986923372260274801316091345126141549522285839402701823884690004497674132615520871839943084040979940198142892825326110513041581064388583488930891380942485307732666485384523705852790683809812073738758055115293090635233887206040961042759996972844810891420692117353333665907710709020698487019805669782598004799421226356372885464480818196786256472944761036204897548977647880837284232444863230958576095091824226426501119748518640709592225529707891969295441026284304137606735506294604060549102824977720776272463738349154440565501914642111802044575388635071779775767726626682303495430936326809"
-- main = print $ take 8 $ phases input !! 100

-- Part 2

processFFT2 :: String -> String
processFFT2 n = digitsToString [processFFT2' n i | i <- [0..len]]
  where
    processFFT2' n i = (abs $ sum $ (zipWith (*) (nums) (patternForRow i))) `mod` 10
    nums = map digitToInt n
    len = (length nums) - 1

phases2 :: String -> [String]
phases2 = iterate processFFT2

input = "1234567812345678"
result = take 5 $ phases2 input
main = do
    print $ result
--     print $ take 8 $ drop 5970807 (cycle result)