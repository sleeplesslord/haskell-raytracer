module Ray where
import Vector

data Ray = Ray {
    origin :: Vector3,
    direction :: Vector3
} deriving Show

atT :: Double -> Ray -> Vector3
atT t r = (origin r) +/ ((direction r) */ t)
