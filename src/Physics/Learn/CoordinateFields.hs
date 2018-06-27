{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Safe #-}

{- | 
Module      :  Physics.Learn.CoordinateFields
Copyright   :  (c) Scott N. Walck 2012-2018
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

Coordinate fields for Cartesian, cylindrical, and spherical coordinates.
-}

module Physics.Learn.CoordinateFields
    ( x
    , y
    , z
    , s
    , phi
    , r
    , theta
    )
    where

import Physics.Learn.Position
    ( ScalarField
    , cartesianCoordinates
    , cylindricalCoordinates
    , sphericalCoordinates
    )

fst3 :: (a,b,c) -> a
fst3 (v,_,_) = v

snd3 :: (a,b,c) -> b
snd3 (_,v,_) = v

thd3 :: (a,b,c) -> c
thd3 (_,_,v) = v

-- | The x Cartesian coordinate of a position.
x :: ScalarField
x = fst3 . cartesianCoordinates

-- | The y Cartesian coordinate of a position.
y :: ScalarField
y = snd3 . cartesianCoordinates

-- | The z Cartesian (or cylindrical) coordinate of a position.
z :: ScalarField
z = thd3 . cartesianCoordinates

-- | The s cylindrical coordinate of a position.
--   This is the distance of the position from the z axis.
s :: ScalarField
s = fst3 . cylindricalCoordinates

-- | The phi cylindrical (or spherical) coordinate of a position.
--   This is the angle from the positive x axis 
--   to the projection of the position onto the xy plane.
phi :: ScalarField
phi = snd3 . cylindricalCoordinates

-- | The r spherical coordinate of a position.
--   This is the distance of the position from the origin.
r :: ScalarField
r = fst3 . sphericalCoordinates

-- | The theta spherical coordinate of a position.
--   This is the angle from the positive z axis to the position.
theta :: ScalarField
theta = snd3 . sphericalCoordinates

