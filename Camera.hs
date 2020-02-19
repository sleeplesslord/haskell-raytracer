module Camera where
    
import Ray hiding (origin)
import Vector

data Camera = Camera {
    lowerLeft :: Vector3,
    origin :: Vector3,
    horizontal :: Vector3,
    vertical :: Vector3
} deriving Show

getRay :: Camera -> Double -> Double -> Ray
getRay camera u v = Ray (origin camera) ((lowerLeft camera) +/ (((horizontal camera) */ u) +/ ((vertical camera) */ v)))
