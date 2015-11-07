{-# OPTIONS_GHC -Wall #-}

-- | Some tools related to the not-gloss 3D graphics and animation library.

module Physics.Learn.Visual.VisTools
    ( v3FromVec
    , v3FromPos
    , visVec
    , oneVector
    , displayVectorField
    , curveObject
    )
    where

import SpatialMath
    ( V3(..)
    , Euler(..)
    )
import Vis
    ( VisObject(..)
    , Color
    )
import Physics.Learn.CarrotVec
    ( Vec
    , xComp
    , yComp
    , zComp
--    , magnitude
    , (^/)
    )
import Physics.Learn.Position
    ( Position
    , cartesianCoordinates
    , VectorField
    )
import Physics.Learn.Curve
    ( Curve(..)
    )

-- | Make a 'V3' object from a 'Vec'.
v3FromVec :: Vec -> V3 Double
v3FromVec v = V3 x y z
    where
      x = xComp v
      y = yComp v
      z = zComp v

-- | Make a 'V3' object from a 'Position'.
v3FromPos :: Position -> V3 Double
v3FromPos r = V3 x y z
    where
      (x,y,z) = cartesianCoordinates r

-- | Display a vector field.
displayVectorField :: Color             -- ^ color for the vector field
                   -> Double            -- ^ scale factor
                   -> [Position]        -- ^ list of positions to show the field
                   -> VectorField       -- ^ vector field to display
                   -> VisObject Double  -- ^ the displayable object
displayVectorField col unitsPerMeter samplePts field
    = VisObjects [Trans (v3FromPos r) $ visVec col (e ^/ unitsPerMeter) | r <- samplePts, let e = field r]

-- | A displayable VisObject for a curve.
curveObject :: Color -> Curve -> VisObject Double
curveObject color (Curve f a b)
    = Line' Nothing [(v3FromPos (f t), color) | t <- [a,a+(b-a)/1000..b]]

-- | Place a vector at a particular position.
oneVector :: Color -> Position -> Vec -> VisObject Double
oneVector c r v = Trans (v3FromPos r) $ visVec c v

data Cart = Cart Double Double Double
            deriving (Show)

data Sph = Sph Double Double Double
           deriving (Show)

sphericalCoords :: Cart -> Sph
sphericalCoords (Cart x y z) = Sph r theta phi
    where
      r     = sqrt (x*x + y*y + z*z)
      s     = sqrt (x*x + y*y)
      theta = atan2 s z
      phi   = atan2 y x

-- | A VisObject arrow from a vector
visVec :: Color -> Vec -> VisObject Double
visVec c v = rotZ phi $ rotY theta $ Arrow (r,20*r) (V3 0 0 1) c
    where
      x = xComp v
      y = yComp v
      z = zComp v
      Sph r theta phi = sphericalCoords (Cart x y z)

{-
rotX :: Double  -- ^ in radians
     -> VisObject Double
     -> VisObject Double
rotX alpha = RotEulerRad (Euler 0 0 alpha)
-}

rotY :: Double  -- ^ in radians
     -> VisObject Double
     -> VisObject Double
rotY alpha = RotEulerRad (Euler 0 alpha 0)

rotZ :: Double  -- ^ in radians
     -> VisObject Double
     -> VisObject Double
rotZ alpha = RotEulerRad (Euler alpha 0 0)


{-
adjacentDistance :: [Position] -> Double
adjacentDistance []         = 0
adjacentDistance rs'@(_:rs) = minimum (map magnitude $ zipWith displacement rs' rs)

visVectorField :: Color -> [Position] -> VectorField -> VisObject Double
visVectorField c rs vf = let prs = [(r,vf r) | r <- rs]
                             bigV = maximum [magnitude (snd pr) | pr <- prs]
                             disp = adjacentDistance rs
                             scaleFactor = disp / bigV
                             newPrs = [(r, scaleFactor *^ v) | (r,v) <- prs]
                             vecs = [oneVector c r v' | (r,v') <- newPrs]
                         in VisObjects vecs
-}
