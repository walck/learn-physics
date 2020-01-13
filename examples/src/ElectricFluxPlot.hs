{-# OPTIONS_GHC -Wall #-}

-- | Electric flux plot.  Load this file into GHCi and ask for plot1.

module Main where

import Physics.Learn.Charge
import Physics.Learn.Surface
import Physics.Learn.Position
import Graphics.Gnuplot.Simple

-- | A plot of electric flux produced by a 1-nC point charge at (x,y,z) = (x,0,0)
--   through a sphere of radius 2 m centered at the origin
--   as a function of x.
plot1 :: IO ()
plot1 = plotFunc [Title "Electric flux produced by a 1-nC point charge through a sphere with radius 2m"
                 ,YLabel "Electric flux (V m)"
                 ,XLabel "Displacement of point charge from center of sphere (m)"
                 ,Key Nothing
                 ]
        [-3.05,-2.95..3] (\x -> electricFlux (sphere 2 (cart 0 0 0)) (PointCharge 1e-9 (cart x 0 0)))

-- | Electric flux plot
main :: IO ()
main = plot1
