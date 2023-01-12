module Main where

import Physics.Learn
import SpatialMath
  ( Euler (..)
  )
import Vis

drawFunction :: SimpleState -> VisObject Double
drawFunction (_t, r, _v) =
  RotEulerDeg (Euler 270 0 0) $
    RotEulerDeg (Euler 0 180 0) $
      VisObjects
        [ Axes (0.5, 15)
        , Trans (v3FromPos r) (Sphere 0.1 Solid red)
        ]

statePropagationFunction :: Float -> SimpleState -> SimpleState
statePropagationFunction t' (t, r, v) = rungeKutta4 newton2 (realToFrac t' - t) (t, r, v)

-- Newton's Second Law
newton2 :: SimpleState -> Diff SimpleState
newton2 (t, r, v) = (1, v, force (t, r, v) ^/ m)

-- Lorentz Force Law
force :: SimpleState -> Vec
force (_t, r, v) = q *^ (electricField r ^+^ v >< magneticField r)

myOptions :: Options
myOptions = defaultOpts {optWindowName = "Particle Experiencing Electromagnetic Force"}

main :: IO ()
main =
  simulate
    myOptions
    0.01
    (0, initialPosition, initialVelocity)
    drawFunction
    statePropagationFunction

-- particle mass
m :: Double
m = 1

-- particle charge
q :: Double
q = 1

-- Electric Field
electricField :: VectorField
electricField r = vec 0 2 0
  where
    (x, y, z) = cartesianCoordinates r

-- Magnetic Field
magneticField :: VectorField
magneticField r = vec 0 0 4
  where
    (x, y, z) = cartesianCoordinates r

-- Initial displacement
initialPosition :: Position
initialPosition = cart 0 0 0

-- Initial velocity
initialVelocity :: Vec
initialVelocity = vec 0 0 0
