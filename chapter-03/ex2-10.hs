import Data.List (sortBy)
import Data.Function (on)

data Direction = TurnLeft | TurnRight | GoStraight
    deriving (Show, Eq)

type Point = (Double, Double)
type Vector = (Double, Double)

turn :: Point -> Point -> Point -> Direction
turn (xa, ya) (xb, yb) (xc, yc)
    | expr == 0 = GoStraight
    | expr > 0  = TurnLeft
    | otherwise = TurnRight
    where expr = x1 * y2 - x2 * y1
          (x1, y1) = (xb - xa, yb - ya)
          (x2, y2) = (xc - xb, yc - yb)

turnList :: [Point] -> [Direction]
turnList (x:y:z:ps) = turn x y z : turnList (y:z:ps)
turnList _ = []

vectorLength :: Vector -> Double
vectorLength (x, y) = sqrt (x**2 + y**2)

angleBetweenVectors :: Vector -> Vector -> Double
angleBetweenVectors v1@(x1, y1) v2@(x2, y2) =
    acos $ (x1 * x2 - y1 * y2) / (vectorLength v1 * vectorLength v2)

angleWithXAxis :: Point -> Point -> Double
angleWithXAxis (xa, ya) (xb, yb) =
    angleBetweenVectors (1, 0) (xb - xa, yb - ya)

convexHull :: [Point] -> [Point]
convexHull points = aux [p1, p] sorted
    where findFirstPoint :: [Point] -> Point
          findFirstPoint pts = (minX, minY)
            where minY = minimum $ map snd pts
                  pointsWithMinY = filter ((== minY) . snd) pts
                  minX = minimum $ map snd pointsWithMinY
          p = findFirstPoint points
          (p1:sorted) = sortBy (compare `on` angleWith) (filter (/= p) points)
          angleWith = angleWithXAxis p
          aux :: [Point] -> [Point] -> [Point]
          aux acc [] = reverse acc
          aux acc@(y:x:xs) (pt:pts)
            | dir == TurnLeft   = aux (pt:acc) pts
            | dir == GoStraight = aux (pt:x:xs) pts
            | otherwise         = aux (rewind $ pt:acc) pts
            where dir = turn x y pt
          rewind :: [Point] -> [Point]
          rewind pts@[_,_] = pts
          rewind pts@(z:y:x:xs)
            | dir == TurnLeft   = pts
            | dir == GoStraight = z:x:xs
            | otherwise         = rewind (z:x:xs)
            where dir = turn x y z
          rewind _ = error "rewind on list with fewer than 2 elements"

main :: IO ()
main = return ()
