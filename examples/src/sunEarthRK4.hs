{-# OPTIONS_GHC -Wall #-}

-- Animation of Earth orbiting around a fixed Sun
-- Using SI units

module Main where

import Physics.Learn
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

type Acceleration = Vec

gGrav :: Double
gGrav = 6.67e-11

massSun :: Double
massSun = 1.99e30

-- This is enlarged so we can see it.
radiusSun :: Double
radiusSun = 0.1 * earthSunDistance

-- This is enlarged so we can see it.
radiusEarth :: Double
radiusEarth = 0.05 * earthSunDistance

earthSunDistance :: Double
earthSunDistance = 1.496e11

year :: Double
year = 365.25*24*60*60

-- Derived constants

initialEarthSpeed :: Double
initialEarthSpeed = 2*pi*earthSunDistance/year

initialState :: SimpleState
initialState = (0
               ,cart earthSunDistance 0 0
               ,vec 0 initialEarthSpeed 0)

rS :: Position
rS = cart 0 0 0

earthGravity :: SimpleAccelerationFunction
earthGravity (_,rE,_)
    = ((-gGrav) * massSun) *^ disp ^/ magnitude disp ** 3
      where
        disp = displacement rS rE

diskPic :: Double -> Picture
diskPic r = ThickCircle (radius/2) radius
    where radius = realToFrac r

-- A yellow disk will represent the Sun
yellowDisk :: Picture
yellowDisk = Color yellow (diskPic radiusSun)

-- A blue disk will represent the Earth
blueDisk :: Picture
blueDisk = Color blue (diskPic radiusEarth)

worldToPicture :: SimpleState -> Picture
worldToPicture (_,rE,_)
    = scale scl scl $ pictures [yellowDisk
                               ,translate xE yE blueDisk
                               ]
    where
      xE = realToFrac x
      yE = realToFrac y
      scl = 200 / realToFrac (earthSunDistance)
      (x,y,_) = cartesianCoordinates rE

timeScale :: Double
timeScale = 0.25 * year

simStep :: ViewPort -> Float -> SimpleState -> SimpleState
simStep _ dt = simpleRungeKuttaStep earthGravity dtScaled
    where
      dtScaled = timeScale * realToFrac dt

main :: IO ()
main = simulate (InWindow "Sun-Earth Animation" (1024, 768) (0, 0))
       black 50 initialState worldToPicture simStep
