{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Safe #-}

{- | 
Module      :  Physics.Learn.CoordinateSystem
Copyright   :  (c) Scott N. Walck 2012-2018
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

A module for working with coordinate systems.
-}

module Physics.Learn.CoordinateSystem
    ( CoordinateSystem(..)
    , standardCartesian
    , standardCylindrical
    , standardSpherical
    , newCoordinateSystem
    )
    where

import Physics.Learn.Position
    ( Position
    , cartesian
    , cartesianCoordinates
    , cylindrical
    , cylindricalCoordinates
    , spherical
    , sphericalCoordinates
    )

-- | Specification of a coordinate system requires
--   a map from coordinates into space, and
--   a map from space into coordinates.
data CoordinateSystem
    = CoordinateSystem { toPosition   :: (Double,Double,Double) -> Position  -- ^ a map from coordinates into space
                       , fromPosition :: Position -> (Double,Double,Double)  -- ^ a map from space into coordinates
                       }

-- | The standard Cartesian coordinate system
standardCartesian :: CoordinateSystem
standardCartesian = CoordinateSystem cartesian cartesianCoordinates

-- | The standard cylindrical coordinate system
standardCylindrical :: CoordinateSystem
standardCylindrical = CoordinateSystem cylindrical cylindricalCoordinates

-- | The standard spherical coordinate system
standardSpherical :: CoordinateSystem
standardSpherical = CoordinateSystem spherical sphericalCoordinates

-- | Define a new coordinate system in terms of an existing one.
--   First parameter is a map from old coordinates to new coordinates.
--   Second parameter is the inverse map from new coordinates to old coordinates.
newCoordinateSystem :: ((Double,Double,Double) -> (Double,Double,Double))  -- ^ (x',y',z') = f(x,y,z)
                    -> ((Double,Double,Double) -> (Double,Double,Double))  -- ^ (x,y,z) = g(x',y',z')
                    -> CoordinateSystem  -- ^ old coordinate system
                    -> CoordinateSystem
newCoordinateSystem f g (CoordinateSystem tp fp)
    = CoordinateSystem (tp . g) (f . fp)
