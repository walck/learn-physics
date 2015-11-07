{-# OPTIONS_GHC -Wall #-}

module Main where

import Vis
    ( animate
    , VisObject(..)
    , red
    , blue
    , Options(..)
    , defaultOpts
    )
import Physics.Learn.CarrotVec
    ( vec
    )
import Physics.Learn
    ( Position
    , VectorField
    , displayVectorField
    , cart
    , cartesianCoordinates
    )

samplePoints :: [Position]
samplePoints = [cart x y z | x <- [-2,0,2], y <- [-2,0,2], z <- [-4,-3.6..4]]

drawFun :: Float -> VisObject Double
drawFun time = VisObjects [displayVectorField blue 1 samplePoints (eField t)
                          ,displayVectorField red  1 samplePoints (bField t)
                          ]
    where
      t = realToFrac time

eField :: Double -> VectorField
eField t r = vec (cos (z - t)) 0 0
    where
      (_,_,z) = cartesianCoordinates r

bField :: Double -> VectorField
bField t r = vec 0 (cos (z - t)) 0
    where
      (_,_,z) = cartesianCoordinates r

myOptions :: Options
myOptions = defaultOpts {optWindowName = "Plane Wave"}

main :: IO ()
main = animate myOptions drawFun
