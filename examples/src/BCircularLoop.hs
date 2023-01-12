{-# OPTIONS_GHC -Wall #-}

module Main where

import Physics.Learn
import Vis

loopCurve :: Curve
loopCurve = Curve (\phi -> cyl 1 phi 0) 0 (2 * pi)

loop :: CurrentDistribution
loop = LineCurrent 20 loopCurve

samplePoints :: [Position]
samplePoints =
  [ cyl s phi z
  | s <- [0.25, 0.75 .. 1.75]
  , phi <- [pi / 6, pi / 2 .. 2 * pi]
  , z <- [-1.5, -1 .. 1.5]
  ]

arrows :: VisObject Double
arrows = displayVectorField blue 5e-5 samplePoints (bField loop)

drawFun :: VisObject Double
drawFun = VisObjects [curveObject red loopCurve, arrows]

myOptions :: Options
myOptions = defaultOpts {optWindowName = "Magnetic Field from a Current Loop"}

main :: IO ()
main = display myOptions drawFun
