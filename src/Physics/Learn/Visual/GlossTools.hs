{-# OPTIONS_GHC -Wall #-}

-- | Some tools related to the gloss 2D graphics and animation library.

module Physics.Learn.Visual.GlossTools
    ( polarToCart
    , cartToPolar
    , arrow
    , thickArrow
    )
    where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

-- positive x is to the right in Translate
-- positive y is up           in Translate (this is good)

basicArrow100 :: Picture
basicArrow100 = Pictures [Line [(0,0),(100,0)],Polygon [(75,5),(100,0),(75,-5)]]

-- | assumes radians coming in
polarToCart :: (Float,Float) -> (Float,Float)
polarToCart (r,theta) = (r * cos theta,r * sin theta)

-- | theta=0 is positive x axis,
--   output angle in radians
cartToPolar :: (Float,Float) -> (Float,Float)
cartToPolar (x,y) = (sqrt (x**2+y**2),atan2 y x)

-- | An arrow
arrow :: Point -- ^ location of base of arrow
      -> Point -- ^ displacement vector
      -> Picture
arrow (x,y) val = Translate x y $ originArrow val

-- | Rotate takes its angle in degrees, and rotates clockwise.
originArrow :: Point  -- ^ displacement vector
            -> Picture
originArrow (x,y)
    = Rotate (-radToDeg theta) $ Scale (r/100) (r/100) basicArrow100
      where
        (r,theta) = cartToPolar (x,y)

basicThickArrow :: Float -> Float -> Float -> Float -> Picture
basicThickArrow l w headLength headWidth
    = Pictures [Polygon [(0,w/2),(l-hl,w/2),(l-hl,-w/2),(0,-w/2)]
               ,Polygon [(l-hl,hw/2),(l,0),(l-hl,-hw/2)]
               ]
    where
      hl = min l headLength
      hw = max w headWidth

-- | A think arrow
thickArrow :: Float -- ^ arrow thickness
           -> Point -- ^ location of base of arrow
           -> Point -- ^ displacement vector
           -> Picture
thickArrow t (x,y) disp
    = Translate x y $ Rotate (-radToDeg theta) $ basicThickArrow r t (r/4) (2*t)
      where
        (r,theta) = cartToPolar disp

