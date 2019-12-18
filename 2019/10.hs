import Data.Char
import Data.List
import Data.Ord
import System.IO

data Point = Point Int Int
    deriving (Show)  

points :: String -> [Point]
points s = astPts
  where
    inputWithoutLines = filter(/='\n') s
    rows = lines s
    xMax = length $ head rows
    yMax = length rows
    ys   = concatMap (take xMax . repeat) [0..yMax-1]
    xs   = cycle [0..xMax-1]
    pts  = map (\(x, y) -> Point x y) (zip xs ys)
    astPts  = map fst (filter ((=='#') . snd) (zip pts inputWithoutLines))

------------------------------------------------------------------------
-- Part 1
------------------------------------------------------------------------
    
calcAngle :: Point -> Double
calcAngle (Point x y) = atan2 (fromIntegral y) (fromIntegral x)

normalize :: Point -> Point -> Point
normalize (Point x1 y1) (Point x2 y2) = Point (x2 - x1) (y2 - y1)

part1 :: [Point] -> (Point, Int)
part1 pts = maximumBy (comparing snd) (map station pts)
  where
    station o = (o, numAsteroids o)
    numAsteroids o = length $ nub $ map (calcAngle . normalize o) pts
    
------------------------------------------------------------------------
-- Part 2
------------------------------------------------------------------------

type RaySegment = (Double, Point)
type Ray = (Double, [Point])

-- rays :: [Point] -> Point -> [Ray]
rays pts o = unsortedRays
-- rays pts o = (sortBy (comparing fst) posRays) ++ (reverse (sortBy (comparing fst) negRays))
  where
    -- unsortedRays = (map (toRay) $ (groupBy (\x y -> fst x == fst y) (rayPieces pts o)))
    unsortedRays = groupBy (\x y -> fst x == fst y) (rayPieces pts o)
    -- posRays = filter ((0<=) . fst) unsortedRays
    -- negRays = filter ((<0) . fst) unsortedRays
    toRay segs = (fst $ head segs, map snd segs)
    rayPieces pts o = map (rayPiece o) pts
    rayPiece o pt= (calcAngle $ normalize o pt, pt)

part2 :: [Ray] -> [Point]
part2 = concat . transpose . map snd 

main :: IO ()
main = do 
  input <- getContents
  let pts = points input
  print $ show $ part1 pts
  print $ show $ rays pts (Point 11 19)
  