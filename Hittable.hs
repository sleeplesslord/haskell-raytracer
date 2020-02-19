module Hittable where 

import Ray
import Vector
import Data.Maybe
import Control.Monad
import Data.List

data HitInfo = HitInfo {
    distance :: Double,
    normal :: Vector3,
    material :: Vector3,
    reflexive :: Bool
} deriving Show

class Hittable h where
    hit :: Ray -> h -> Maybe HitInfo

data Sphere = Sphere {
    center :: Vector3,
    radius :: Double,
    color :: Vector3,
    sphereIsReflexive :: Bool
}

instance Hittable Sphere where
    hit ray sphere
      | discriminant < 0 = Nothing
      | distance <= 0.0001 = if altDistance > 0.0001 then Just $ HitInfo altDistance normal (color sphere) (sphereIsReflexive sphere) else Nothing
      | otherwise = Just $ HitInfo {distance = distance, normal = normal, material = (color sphere), reflexive = (sphereIsReflexive sphere)}
        where
            oc = (origin ray) -/ (center sphere)
            a = (direction ray) `dot` (direction ray)
            b = (dot oc (direction ray))
            c = (oc `dot` oc) - ((radius sphere) * (radius sphere))
            discriminant = b * b - a * c
            normal = normalize $ ((direction ray) */ distance) +/ (origin ray) -/ (center sphere)
            distance = ((-b) - (sqrt discriminant)) / a 
            altDistance = ((-b) + (sqrt discriminant)) / a

data Plane = Plane {
    planeNormal :: Vector3,
    point :: Vector3,
    planeColor :: Vector3
}

instance Hittable Plane where
    hit ray plane
      | (planeNormal plane) `dot` (direction ray) == 0 = Nothing
      | dist > 10 = Nothing
      | otherwise = Just HitInfo { distance = dist, normal = (planeNormal plane), material = (planeColor plane), reflexive = False}
          where
              dist = ((planeNormal plane) `dot` (point plane) - ((planeNormal plane) `dot` (origin ray))) / ((planeNormal plane) `dot` (direction ray))

getClosestHit :: [Maybe HitInfo] -> Maybe HitInfo
getClosestHit = fmap (minimumBy (\x y -> distance x `compare` distance y)) . mfilter (not . null) . sequence . filter isJust
