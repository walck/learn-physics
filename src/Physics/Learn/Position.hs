{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE Safe #-}

{- | 
Module      :  Physics.Learn.Position
Copyright   :  (c) Scott N. Walck 2012-2018
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

A module for working with the idea of position and coordinate systems.
-}

module Physics.Learn.Position
    ( Position
    , Displacement
    , ScalarField
    , VectorField
    , Field
    , CoordinateSystem
    , cartesian
    , cylindrical
    , spherical
    , cart
    , cyl
    , sph
    , cartesianCoordinates
    , cylindricalCoordinates
    , sphericalCoordinates
    , displacement
    , shiftPosition
    , shiftObject
    , shiftField
    , addFields
    , rHat
    , thetaHat
    , phiHat
    , sHat
    , xHat
    , yHat
    , zHat
    )
    where

import Data.VectorSpace
    ( AdditiveGroup
    )
import Physics.Learn.CarrotVec
    ( Vec
    , vec
    , xComp
    , yComp
    , zComp
    , iHat
    , jHat
    , kHat
    , sumV
    , magnitude
    , (^/)
    )

-- | A type for position.
--   Position is not a vector because it makes no sense to add positions.
data Position = Cart Double Double Double
                deriving (Show)

-- | A displacement is a vector.
type Displacement = Vec

-- | A scalar field associates a number with each position in space.
type ScalarField = Position -> Double

{-
-- | Scalar fields can be added, subtracted, multiplied, and negated,
--   just like scalars themselves.
instance Num ScalarField where
    (f + g) x = f x + g x
    (f * g) x = f x * g x
    (f - g) x = f x - g x
    negate f x = negate (f x)
    abs f x = abs (f x)
    signum f x = signum (f x)
    fromInteger n = const (fromInteger n)

-- | Scalar fields can be divided, just like scalars themselves.
instance Fractional ScalarField where
    (f / g) x = f x / g x
    recip f x = recip (f x)
    fromRational rat = const (fromRational rat)

-- | Cosine of a scalar field, etc.
instance Floating ScalarField where
    pi = const pi
    exp f x = exp (f x)
    sqrt f x = sqrt (f x)
    log f x = log (f x)
    (f ** g) x = f x ** g x
    logBase f g x = logBase (f x) (g x)
    sin f x = sin (f x)
    cos f x = cos (f x)
    tan f x = tan (f x)
    asin f x = asin (f x)
    acos f x = acos (f x)
    atan f x = atan (f x)
    sinh f x = sinh (f x)
    cosh f x = cosh (f x)
    tanh f x = tanh (f x)
    asinh f x = asinh (f x)
    acosh f x = acosh (f x)
    atanh f x = atanh (f x)
-}

-- | A vector field associates a vector with each position in space.
type VectorField = Position -> Vec

-- | Sometimes we want to be able to talk about a field without saying
--   whether it is a scalar field or a vector field.
type Field v     = Position -> v

-- | A coordinate system is a function from three parameters to space.
type CoordinateSystem = (Double,Double,Double) -> Position

-- | Add two scalar fields or two vector fields.
addFields :: AdditiveGroup v => [Field v] -> Field v
addFields flds r = sumV [fld r | fld <- flds]

-- | The Cartesian coordinate system.  Coordinates are (x,y,z).
cartesian :: CoordinateSystem
cartesian (x,y,z) = Cart x y z

-- | The cylindrical coordinate system.  Coordinates are (s,phi,z),
--   where s is the distance from the z axis and phi is the angle
--   with the x axis.
cylindrical :: CoordinateSystem
cylindrical (s,phi,z) = Cart (s * cos phi) (s * sin phi) z

-- | The spherical coordinate system.  Coordinates are (r,theta,phi),
--   where r is the distance from the origin, theta is the angle with
--   the z axis, and phi is the azimuthal angle.
spherical :: CoordinateSystem
spherical (r,th,phi) = Cart (r * sin th * cos phi) (r * sin th * sin phi) (r * cos th)

-- | A helping function to take three numbers x, y, and z and form the
--   appropriate position using Cartesian coordinates.
cart :: Double  -- ^ x coordinate
     -> Double  -- ^ y coordinate
     -> Double  -- ^ z coordinate
     -> Position
cart = Cart

-- | A helping function to take three numbers s, phi, and z and form the
--   appropriate position using cylindrical coordinates.
cyl :: Double  -- ^ s coordinate
    -> Double  -- ^ phi coordinate
    -> Double  -- ^ z coordinate
    -> Position
cyl s phi z = Cart (s * cos phi) (s * sin phi) z

-- | A helping function to take three numbers r, theta, and phi and form the
--   appropriate position using spherical coordinates.
sph :: Double  -- ^ r coordinate
    -> Double  -- ^ theta coordinate
    -> Double  -- ^ phi coordinate
    -> Position
sph r theta phi = Cart (r * sin theta * cos phi) (r * sin theta * sin phi) (r * cos theta)

-- | Returns the three Cartesian coordinates as a triple from a position.
cartesianCoordinates :: Position -> (Double,Double,Double)
cartesianCoordinates (Cart x y z) = (x,y,z)

-- | Returns the three cylindrical coordinates as a triple from a position.
cylindricalCoordinates :: Position -> (Double,Double,Double)
cylindricalCoordinates (Cart x y z) = (s,phi,z)
    where
      s = sqrt(x**2 + y**2)
      phi = atan2 y x

-- | Returns the three spherical coordinates as a triple from a position.
sphericalCoordinates :: Position -> (Double,Double,Double)
sphericalCoordinates (Cart x y z) = (r,theta,phi)
    where
      r = sqrt(x**2 + y**2 + z**2)
      theta = atan2 s z
      s = sqrt(x**2 + y**2)
      phi = atan2 y x

-- | Displacement from source position to target position.
displacement :: Position  -- ^ source position
             -> Position  -- ^ target position
             -> Displacement
displacement (Cart x' y' z') (Cart x y z) = vec (x-x') (y-y') (z-z')

-- | Shift a position by a displacement.
shiftPosition :: Displacement -> Position -> Position
shiftPosition v (Cart x y z) = Cart (x + xComp v) (y + yComp v) (z + zComp v)

-- | An object is a map into 'Position'.
shiftObject :: Displacement -> (a -> Position) -> (a -> Position)
shiftObject d f = shiftPosition d . f

-- | A field is a map from 'Position'.
shiftField :: Displacement -> (Position -> v) -> (Position -> v)
shiftField d f = f . shiftPosition d

-- | The vector field in which each point in space is associated
--   with a unit vector in the direction of increasing spherical coordinate
--   r, while spherical coordinates theta and phi
--   are held constant.
--   Defined everywhere except at the origin.
--   The unit vector 'rHat' points in different directions at different points
--   in space.  It is therefore better interpreted as a vector field, rather
--   than a vector.
rHat :: VectorField
rHat rv = d ^/ magnitude d
    where
      d = displacement (cart 0 0 0) rv

-- | The vector field in which each point in space is associated
--   with a unit vector in the direction of increasing spherical coordinate
--   theta, while spherical coordinates r and phi are held constant.
--   Defined everywhere except on the z axis.
thetaHat :: VectorField
thetaHat r = vec (cos theta * cos phi) (cos theta * sin phi) (-sin theta)
    where
      (_,theta,phi) = sphericalCoordinates r

-- | The vector field in which each point in space is associated
--   with a unit vector in the direction of increasing (cylindrical or spherical) coordinate
--   phi, while cylindrical coordinates s and z
--   (or spherical coordinates r and theta) are held constant.
--   Defined everywhere except on the z axis.
phiHat :: VectorField
phiHat r = vec (-sin phi) (cos phi) 0
    where
      (_,phi,_) = cylindricalCoordinates r

-- | The vector field in which each point in space is associated
--   with a unit vector in the direction of increasing cylindrical coordinate
--   s, while cylindrical coordinates phi and z
--   are held constant.
--   Defined everywhere except on the z axis.
sHat :: VectorField
sHat r = vec (cos phi) (sin phi) 0
    where
      (_,phi,_) = cylindricalCoordinates r

-- | The vector field in which each point in space is associated
--   with a unit vector in the direction of increasing Cartesian coordinate
--   x, while Cartesian coordinates y and z
--   are held constant.
--   Defined everywhere.
xHat :: VectorField
xHat = const iHat

-- | The vector field in which each point in space is associated
--   with a unit vector in the direction of increasing Cartesian coordinate
--   y, while Cartesian coordinates x and z
--   are held constant.
--   Defined everywhere.
yHat :: VectorField
yHat = const jHat

-- | The vector field in which each point in space is associated
--   with a unit vector in the direction of increasing Cartesian coordinate
--   z, while Cartesian coordinates x and y
--   are held constant.
--   Defined everywhere.
zHat :: VectorField
zHat = const kHat

