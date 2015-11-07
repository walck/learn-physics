{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Trustworthy #-}

{- | 
Module      :  Physics.Learn.Volume
Copyright   :  (c) Scott N. Walck 2012-2014
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

This module contains functions for working with 'Volume's
and volume integrals over 'Volume's.
-}

module Physics.Learn.Volume
    (
    -- * Volumes
      Volume(..)
    , unitBall
    , unitBallCartesian
    , centeredBall
    , ball
    , northernHalfBall
    , centeredCylinder
    , shiftVolume
    -- * Volume Integral
    , volumeIntegral
    )
    where

import Data.VectorSpace
    ( VectorSpace
    , InnerSpace
    , Scalar
    )
import Physics.Learn.CarrotVec
    ( Vec
    , vec
    , sumV
    , (^+^)
    , (^-^)
    , (^*)
    , (^/)
    , (<.>)
    , (><)
    , magnitude
    )
import Physics.Learn.Position
    ( Position
    , Displacement
    , Field
    , cartesian
    , cylindrical
    , spherical
    , shiftPosition
    , displacement
    )

-- | Volume is a parametrized function from three parameters to space,
--   lower and upper limits on the first parameter,
--   lower and upper limits for the second parameter
--   (expressed as functions of the first parameter),
--   and lower and upper limits for the third parameter
--   (expressed as functions of the first and second parameters).
data Volume = Volume { volumeFunc :: (Double,Double,Double) -> Position  -- ^ function from 3 parameters to space
                     , loLimit    :: Double                      -- ^ s_a
                     , upLimit    :: Double                      -- ^ s_b
                     , loCurve    :: Double -> Double            -- ^ t_a(s)
                     , upCurve    :: Double -> Double            -- ^ t_b(s)
                     , loSurf     :: Double -> Double -> Double  -- ^ u_a(s,t)
                     , upSurf     :: Double -> Double -> Double  -- ^ u_b(s,t)
                     }

-- | A unit ball, centered at the origin.
unitBall :: Volume
unitBall = Volume spherical 0 1 (const 0) (const pi) (\_ _ -> 0) (\_ _ -> 2*pi)

-- | A unit ball, centered at the origin.  Specified in Cartesian coordinates.
unitBallCartesian :: Volume
unitBallCartesian = Volume cartesian (-1) 1 (\x -> -sqrtTol (1 - x*x)) (\x -> sqrtTol (1 - x*x))
                    (\x y -> -sqrtTol (1 - x*x - y*y)) (\x y -> sqrtTol (1 - x*x - y*y))

-- | A ball with given radius, centered at the origin.
centeredBall :: Double -> Volume
centeredBall a = Volume spherical 0 a (const 0) (const pi) (\_ _ -> 0) (\_ _ -> 2*pi)

-- | Ball with given radius and center.
ball :: Double    -- ^ radius
     -> Position  -- ^ center
     -> Volume    -- ^ ball with given radius and center
ball a c = Volume (\(r,th,phi) -> shiftPosition (vec (r * sin th * cos phi) (r * sin th * sin phi) (r * cos th)) c)
           0 a (const 0) (const pi) (\_ _ -> 0) (\_ _ -> 2*pi)

-- | Upper half ball, unit radius, centered at origin.
northernHalfBall :: Volume
northernHalfBall = Volume spherical 0 1 (const 0) (const (pi/2)) (\_ _ -> 0) (\_ _ -> 2*pi)

-- | Cylinder with given radius and height.  Circular base of the cylinder
--   is centered at the origin.  Circular top of the cylinder lies in plane z = h.
centeredCylinder :: Double  -- radius
                 -> Double  -- height
                 -> Volume  -- cylinder
centeredCylinder r h = Volume cylindrical 0 r (const 0) (const (2*pi)) (\_ _ -> 0) (\_ _ -> h)

-- | A volume integral
volumeIntegral :: (VectorSpace v, Scalar v ~ Double) =>
                  Int          -- ^ number of intervals for first parameter   (s)
               -> Int          -- ^ number of intervals for second parameter  (t)
               -> Int          -- ^ number of intervals for third parameter   (u)
               -> Field v      -- ^ scalar or vector field
               -> Volume       -- ^ the volume
               -> v            -- ^ scalar or vector result
volumeIntegral n1 n2 n3 field (Volume f s_l s_u t_l t_u u_l u_u)
    = sumV $ map sumV $ map (map sumV) (zipCubeWith (^*) aveVals volumes)
      where
        pts = [[[f (s,t,u) | u <- linSpaced n3 (u_l s t) (u_u s t) ] | t <- linSpaced n2 (t_l s) (t_u s)] | s <- linSpaced n1 s_l s_u]
        volumes = zipWith3 (zipWith3 (zipWith3 (\du dv dw -> du <.> (dv >< dw)))) dus dvs dws
        dus = uncurry zipSub3 (shift1 pts)
        dvs = uncurry zipSub3 (shift2 pts)
        dws = uncurry zipSub3 (shift3 pts)
        vals = map (map (map field)) pts
        aveVals = ((uncurry zipAve3 . shift1) . (uncurry zipAve3 . shift2) . (uncurry zipAve3 . shift3)) vals

-- zipSquareWith :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
-- zipSquareWith = zipWith . zipWith

zipCubeWith :: (a -> b -> c) -> [[[a]]] -> [[[b]]] -> [[[c]]]
zipCubeWith = zipWith . zipWith . zipWith

-- zipSub :: [Vec] -> [Vec] -> [Vec]
-- zipSub = zipWith (^-^)

-- zipSub2 :: [[Vec]] -> [[Vec]] -> [[Vec]]
-- zipSub2 = zipWith $ zipWith (^-^)

zipSub3 :: [[[Position]]] -> [[[Position]]] -> [[[Vec]]]
zipSub3 = zipCubeWith displacement

zipAve3 :: (VectorSpace v, Scalar v ~ Double) => [[[v]]] -> [[[v]]] -> [[[v]]]
zipAve3 = zipCubeWith ave

shift1 :: [a] -> ([a],[a])
shift1 pts = (pts, tail pts)

shift2 :: [[a]] -> ([[a]],[[a]])
shift2 pts2d = (pts2d, map tail pts2d)

shift3 :: [[[a]]] -> ([[[a]]],[[[a]]])
shift3 pts3d = (pts3d, map (map tail) pts3d)

-- | n+1 points
linSpaced :: Int -> Double -> Double -> [Double]
linSpaced n a b
    | a < b      = let dx = (b - a) / fromIntegral n
                   in [a,a+dx..b]
    | a ~~ b     = [ave a b]
    | otherwise  = error $ "linSpaced:  lower limit greater than upper limit:  (n,a,b) = " ++ show (n,a,b)

(~~) :: (InnerSpace v, Scalar v ~ Double) => v -> v -> Bool
a ~~ b = magnitude (a ^-^ b) < tolerance

tolerance :: Double
tolerance = 1e-10

ave :: (VectorSpace v, Scalar v ~ Double) => v -> v -> v
ave v1 v2 = (v1 ^+^ v2) ^/ 2

sqrtTol :: Double -> Double
sqrtTol x
    | x >= 0              = sqrt x
    | abs x <= tolerance  = 0
    | otherwise           = error ("sqrtTol:  I can't take the sqrt of " ++ show x)

-- | Shift a volume by a displacement.
shiftVolume :: Displacement -> Volume -> Volume
shiftVolume d (Volume f sl su tl tu ul uu)
    = Volume (shiftPosition d . f) sl su tl tu ul uu
