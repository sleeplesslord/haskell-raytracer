module Main where
import Vector
import Ray
import Hittable
import Camera
import System.IO.Unsafe ( unsafePerformIO )
import Data.Maybe

data World = World {
    hitTest :: (Ray -> Maybe HitInfo),
    lights :: [Vector3]
}

black = Vector3 0 0 0
lightColor = Vector3 1 1 1

castShadowRay :: World -> Vector3 -> Vector3 -> Vector3 -> Vector3
castShadowRay world source normal light = (lightHitCoeff \*) $ fromMaybe lightColor $ do
    (HitInfo distance _ _ _) <- hitTest world rayToLight
    if distance > vecLength vectorToLight
       then 
       Nothing
    else
       Just black
    where
        vectorToLight = light -/ source
        rayToLight = Ray source (normalize vectorToLight)
        -- lightHitCoeff = normalize vectorToLight `dot` normal
        lightHitCoeff = 1

blendColors :: Vector3 -> [Vector3] -> Vector3
blendColors _ [] = black
blendColors (Vector3 x y z) lights = Vector3 (a * x) (b * y) (c * z)
    where 
        (Vector3 a b c) = foldr (\(Vector3 a b c) (Vector3 x y z) -> (Vector3 (a + x) (b + y) (c + z))) (Vector3 0 0 0) lights

calculateHitColor :: World -> Ray -> Int -> HitInfo -> Vector3
calculateHitColor world ray bounces (HitInfo distance normal color reflexive) = 
    if reflexive then
        castRay world (Ray (atT distance ray) (reflect (direction ray) normal)) bounces
    else
        blendColors color (lightColors ++ spreadRayColors)
    where
        hitPoint = atT distance ray
        vectorTo x = x -/ hitPoint
        lightColors = zipWith (\*) lightCoeffs $ map (castShadowRay world hitPoint normal) (lights world)
        lightCoeffs = map ((dot normal) . normalize . vectorTo) (lights world)
        orthogonalOne = if normal `cross` (Vector3 0 1 0) == (Vector3 0 0 0) then (Vector3 1 0 0) else normal `cross` (Vector3 0 1 0)
        orthogonalTwo = normal `cross` orthogonalOne
        coeffs = [0.002,0.001]
        spreadRays = do
            a <- coeffs
            b <- coeffs
            [(normal */ a) +/ (orthogonalOne */ b), (normal */ a) +/ (orthogonalOne */ (-b))]
        -- spreadRays = [normal +/ orthogonalOne, normal -/ orthogonalOne, normal +/ orthogonalTwo, normal -/ orthogonalTwo]
        spreadRayCoeffs = map ((/ (fromIntegral $ 10 * (length spreadRays))) . (dot normal) . normalize) spreadRays
        spreadRayColors = zipWith (\*) spreadRayCoeffs $ map (\x -> castRay world (Ray hitPoint (normalize x)) bounces) spreadRays

skyboxColor :: Ray -> Vector3
skyboxColor (Ray _ (Vector3 a b c)) = (Vector3 (max 0 (a / 2)) (max 0 (b / 2)) ((abs c) / 2))

-- Cast ray takes a world, a ray from X to Y and produces the color of the light that reaches X from Y
castRay :: World -> Ray -> Int -> Vector3
castRay _ _ 0 = Vector3 0 0 0
castRay world ray bounces = fromMaybe (skyboxColor ray) $ fmap (calculateHitColor world ray ( bounces - 1)) ((hitTest world) ray)

main = do
    -- setup image parameters
    let width = 800 :: Int
    let height = 800 :: Int
    let camera = Camera { lowerLeft = (Vector3 (-1) (-1) (-1)), Camera.origin = (Vector3 0 0 3), horizontal = (Vector3 2 0 0), vertical = (Vector3 0 2 0) }

    -- world
    let spheres = [Sphere { center = (Vector3 (-2) 0 0), radius = 1, color = (Vector3 1 1 1), sphereIsReflexive = False}, Sphere { center = (Vector3 2 0.25 0),radius = 1, color = (Vector3 0 1 0), sphereIsReflexive = True }, Sphere { center = (Vector3 0 (-1000) 0), radius = 999, color = Vector3 1 0 1, sphereIsReflexive = False }]
    let hitFunctions = map (flip hit) spheres
    let world = World {hitTest = getClosestHit . sequence hitFunctions, lights = [Vector3 0 4 0]}
    let getColors = (\x -> castRay world x 3)

    putStrLn "P3"
    putStrLn $ (show width) ++ " " ++ (show height)
    print 255
    (sequence . map (putStrLn . showColor)) $ 
        [ getColors $
            getRay camera (fromIntegral x / fromIntegral width) (fromIntegral y / fromIntegral height) | y <- [height-1,height-2..0], x <- [0..width-1]]
