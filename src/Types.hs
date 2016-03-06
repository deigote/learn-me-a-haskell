data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r * 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudgePoint :: Point -> Point -> Point
nudgePoint (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

nudge :: Shape -> Point -> Shape
nudge (Circle p1 r) p2 = Circle (nudgePoint p1 p2) r
nudge (Rectangle p1 p2) p3 = Rectangle (nudgePoint p1 p3) (nudgePoint p2 p3)

main = do
    putStrLn $ show $ surface $ Circle (Point 10 20) 10
    putStrLn $ show $ surface $ Rectangle (Point 0 0) (Point 100 100)
    putStrLn $ show $ nudge (Circle (Point 10 20) 10) (Point 20 30)
    putStrLn $ show $ nudge (Circle (Point 10 20) 10) (Point 20 30)
    putStrLn $ show $ nudge (Rectangle (Point 1 1) (Point 2 2)) (Point 3 7)