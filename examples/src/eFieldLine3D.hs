{-# OPTIONS_GHC -Wall #-}

module Main where

import Physics.Learn.Charge
  ( ChargeDistribution (..)
  , eField
  )
import Physics.Learn.Curve
  ( Curve (..)
  )
import Physics.Learn.Position
  ( Position
  , cart
  )
import Physics.Learn.Visual.VisTools
  ( curveObject
  , displayVectorField
  )
import Vis
  ( Options (..)
  , VisObject (..)
  , blue
  , defaultOpts
  , display
  , red
  )

curve1 :: Curve
curve1 = Curve (\t -> cart t 0 0) (-4) 4

lineCharge :: ChargeDistribution
lineCharge = LineCharge (const 1e-9) curve1

samplePoints :: [Position]
samplePoints = [cart x y z | x <- [-8, -6 .. 8], y <- [-4, -2 .. 4], z <- [-4, -2 .. 4], abs y + abs z > 0.5 || abs x > 4.5]

arrows :: VisObject Double
arrows = displayVectorField blue 10 samplePoints (eField lineCharge)

drawFun :: VisObject Double
drawFun = VisObjects [curveObject red curve1, arrows]

myOptions :: Options
myOptions = defaultOpts {optWindowName = "Electric Field from a Line Charge"}

main :: IO ()
main = display myOptions drawFun
