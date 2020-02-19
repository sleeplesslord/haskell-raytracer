module Vector where 
data Vector3 = Vector3 Double Double Double deriving (Eq, Show)


(+/) :: Vector3 -> Vector3 -> Vector3
(+/) (Vector3 a b c) (Vector3 x y z) = Vector3 (a + x) (b + y) (c + z)

(-/) :: Vector3 -> Vector3 -> Vector3
(-/) (Vector3 a b c) (Vector3 x y z) = Vector3 (a - x) (b - y) (c - z)

(//) :: Vector3 -> Double -> Vector3
(//) (Vector3 a b c) x = (Vector3 (a / x) (b / x) (c / x))

(*/) :: Vector3 -> Double -> Vector3
(*/) v x = v // (1 / x)

(\*) :: Double -> Vector3 -> Vector3
(\*) x v = v // (1 / x)

reflect :: Vector3 -> Vector3 -> Vector3
reflect d n = d -/ ((2 * (d `dot` n)) \* n)

cross :: Vector3 -> Vector3 -> Vector3
cross (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = Vector3 (a2 * b3 - a3 * b2) (a3 * b1 - a1 * b3) (a1* b2 - a2*b1)

toColor :: Vector3 -> Vector3
toColor (Vector3 x y z) = 
    Vector3 (abs $ 255.99 * x) (abs $ 255.99 * y) (abs $ 255.99 * z)

vecLength v = sqrt $ v `dot` v

normalize :: Vector3 -> Vector3
normalize v = v // (vecLength v)

dot :: Vector3 -> Vector3 -> Double
dot (Vector3 a b c) (Vector3 x y z) = a * x + b * y + c * z

showColor (Vector3 a b c) = show (abs $ floor (a * 255)) ++ " " ++ show (abs $ floor (b * 255)) ++ " " ++ show (abs $ floor (c * 255))
