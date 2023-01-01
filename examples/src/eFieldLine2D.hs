{-# OPTIONS_GHC -Wall #-}

module Main where

import Graphics.Gloss
import Physics.Learn.CarrotVec
import Physics.Learn.Charge
import Physics.Learn.Curve
import Physics.Learn.Position
import Physics.Learn.Visual.GlossTools

pixelsPerMeter :: Float
pixelsPerMeter = 40

pixelsPerVPM :: Float
pixelsPerVPM = 5.6

scalePoint :: Float -> Point -> Point
scalePoint m (x, y) = (m * x, m * y)

twoD :: Vec -> Point
twoD r = (realToFrac $ xComp r, realToFrac $ yComp r)

twoDp :: Position -> Point
twoDp r = (realToFrac x, realToFrac y)
  where
    (x, y, _) = cartesianCoordinates r

samplePoints :: [Position]
samplePoints = [cart x y 0 | x <- [-8, -6 .. 8], y <- [-6, -4 .. 6], abs y > 0.5 || abs x > 4.5]

curve1 :: Curve
curve1 = Curve (\t -> cart t 0 0) (-4) 4

eFields :: [(Position, Vec)]
eFields = [(r, eFieldFromLineCharge (const 1e-9) curve1 r) | r <- samplePoints]

arrows :: [Picture]
arrows =
  [ thickArrow
    5
    (scalePoint pixelsPerMeter $ twoDp r)
    (scalePoint pixelsPerVPM $ twoD e)
  | (r, e) <- eFields
  ]

main :: IO ()
main =
  display (InWindow "Electric Field from a Line Charge" (680, 520) (10, 10)) white $
    Pictures
      [ (Color blue (Pictures arrows))
      , Color orange $ Line [(-4 * pixelsPerMeter, 0), (4 * pixelsPerMeter, 0)]
      ]
