{-# OPTIONS_GHC -Wall #-}

module Main where

import Graphics.Gnuplot.Simple
import Physics.Learn

-- type StateTuple = (Double,Vec,Vec)
-- type AccelerationFunction = StateTuple -> Vec

-- eulerCromerSolution :: Double -> AccelerationFunction
--                    -> StateTuple -> StateTuple
-- eulerCromerSolution

-- vertical direction is y direction
earthSurfaceGravity :: OneParticleAccelerationFunction
earthSurfaceGravity _state = vec 0 (-g) 0

g :: Double
g = 9.81

projectileTuples
  :: Double
  -> Double
  -> OneParticleAccelerationFunction
  -> [OneParticleSystemState]
projectileTuples v0 theta af =
  oneParticleRungeKuttaSolution
    af
    0.01
    (0, St (cart 0 0 0) (vec vx0 vy0 0))
  where
    vx0 = v0 * cos theta
    vy0 = v0 * sin theta

yCoord :: Position -> Double
yCoord r = y
  where
    (_, y, _) = cartesianCoordinates r

inAir :: [OneParticleSystemState] -> [OneParticleSystemState]
inAir = takeWhile (\(_, St r _) -> yCoord r >= 0)

initialProjState :: Double -> Double -> OneParticleSystemState
initialProjState v0 theta =
  (0, St (cart 0 0 0) (vec vx0 vy0 0))
  where
    vx0 = v0 * cos theta
    vy0 = v0 * sin theta

-- air resistance quadratic in the speed
surfaceGravityAirResistance
  :: Double
  -> Double
  -> OneParticleAccelerationFunction
surfaceGravityAirResistance m b (_t, St _r v) =
  netForce ^/ m
  where
    netForce = gravity ^+^ airResistance
    gravity = vec 0 (-m * g) 0
    airResistance = ((-b) * magnitude v) *^ v

trajectory :: [OneParticleSystemState] -> [(Double, Double)]
trajectory sts = [(x, y) | (_, St r _) <- sts, let (x, y, _) = cartesianCoordinates r]

traj :: Double -> [(Double, Double)]
traj b =
  trajectory $
    inAir $
      oneParticleRungeKuttaSolution
        (surfaceGravityAirResistance 2 b)
        0.01
        (initialProjState 30 (pi / 6))

main :: IO ()
main =
  plotPathsStyle
    [ Title "Trajectories of 2-kg object, initial speed 30 m/s, angle 30 degrees"
    , XLabel "Range (m)"
    , YLabel "Height (m)"
    , PNG "learn-physics-Projectile.png"
    ]
    [ (defaultStyle {lineSpec = CustomStyle [LineTitle "No air resistance"]}, traj 0)
    , (defaultStyle {lineSpec = CustomStyle [LineTitle "Drag 0.01 kg/m"]}, traj 0.01)
    , (defaultStyle {lineSpec = CustomStyle [LineTitle "Drag 0.02 kg/m"]}, traj 0.02)
    ]
    >> putStrLn "output sent to file learn-physics-Projectile.png"
