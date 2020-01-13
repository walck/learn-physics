{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Safe #-}

{- | 
Module      :  Physics.Learn.Surface
Copyright   :  (c) Scott N. Walck 2012-2019
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

This module contains functions for working with 'Surface's
and surface integrals over 'Surface's.
-}

module Physics.Learn.Surface
    (
    -- * Surfaces
      Surface(..)
    , unitSphere
    , centeredSphere
    , sphere
    , northernHemisphere
    , disk
    , shiftSurface
    -- * Surface Integrals
    , surfaceIntegral
    , dottedSurfaceIntegral
    )
    where

import Data.VectorSpace
    ( VectorSpace
    , InnerSpace
    , Scalar
    )
import Physics.Learn.CarrotVec
    ( vec
    , (^+^)
    , (^-^)
    , (^*)
    , (^/)
    , (<.>)
    , (><)
    , magnitude
    , sumV
    )
import Physics.Learn.Position
    ( Position
    , Displacement
    , VectorField
    , Field
    , cart
    , cyl
    , shiftPosition
    , displacement
    )

-- | Surface is a parametrized function from two parameters to space,
--   lower and upper limits on the first parameter, and
--   lower and upper limits for the second parameter
--   (expressed as functions of the first parameter).
data Surface = Surface { surfaceFunc :: (Double,Double) -> Position  -- ^ function from two parameters (s,t) into space
                       , lowerLimit :: Double            -- ^ s_l
                       , upperLimit :: Double            -- ^ s_u
                       , lowerCurve :: Double -> Double  -- ^ t_l(s)
                       , upperCurve :: Double -> Double  -- ^ t_u(s)
                       }

-- | A unit sphere, centered at the origin.
unitSphere :: Surface
unitSphere = Surface (\(th,phi) -> cart (sin th * cos phi) (sin th * sin phi) (cos th))
             0 pi (const 0) (const $ 2*pi)

-- | A sphere with given radius centered at the origin.
centeredSphere :: Double -> Surface
centeredSphere r = Surface (\(th,phi) -> cart (r * sin th * cos phi) (r * sin th * sin phi) (r * cos th))
                   0 pi (const 0) (const $ 2*pi)

-- | Sphere with given radius and center.
sphere :: Double -> Position -> Surface
sphere r c = Surface (\(th,phi) -> shiftPosition (vec (r * sin th * cos phi) (r * sin th * sin phi) (r * cos th)) c)
             0 pi (const 0) (const $ 2*pi)

-- | The upper half of a unit sphere, centered at the origin.
northernHemisphere :: Surface
northernHemisphere = Surface (\(th,phi) -> cart (sin th * cos phi) (sin th * sin phi) (cos th))
                     0 (pi/2) (const 0) (const $ 2*pi)

-- | A disk with given radius, centered at the origin.
disk :: Double -> Surface
disk radius = Surface (\(s,phi) -> cyl s phi 0) 0 radius (const 0) (const (2*pi))

-- To do : boundaryOfSurface :: Surface -> Curve

-- | A plane surface integral, in which area element is a scalar.
surfaceIntegral :: (VectorSpace v, Scalar v ~ Double) =>
                   Int      -- ^ number of intervals for first parameter, s
                -> Int      -- ^ number of intervals for second parameter, t
                -> Field v  -- ^ the scalar or vector field to integrate
                -> Surface  -- ^ the surface over which to integrate
                -> v        -- ^ the resulting scalar or vector
surfaceIntegral n1 n2 field (Surface f s_l s_u t_l t_u)
    = sumV $ map sumV $ zipWith (zipWith (^*)) aveVals (map (map magnitude) areas)
      where
        pts = [[f (s,t) | t <- linSpaced n2 (t_l s) (t_u s)] | s <- linSpaced n1 s_l s_u]
        areas = zipWith (zipWith (><)) dus dvs
        dus = zipWith (zipWith displacement) pts (tail pts)
        dvs = map (\row -> zipWith displacement row (tail row)) pts
        vals = map (map field) pts
        halfAveVals = map (\row -> zipWith ave (tail row) row) vals
        aveVals = zipWith (zipWith ave) (tail halfAveVals) halfAveVals

-- | A dotted surface integral, in which area element is a vector.
dottedSurfaceIntegral :: Int          -- ^ number of intervals for first parameter, s
                      -> Int          -- ^ number of intervals for second parameter, t
                      -> VectorField  -- ^ the vector field to integrate
                      -> Surface      -- ^ the surface over which to integrate
                      -> Double       -- ^ the resulting scalar
dottedSurfaceIntegral n1 n2 vf (Surface f s_l s_u t_l t_u)
    = sum $ map sum $ zipWith (zipWith (<.>)) aveVals areas
      where
        pts = [[f (s,t) | t <- linSpaced n2 (t_l s) (t_u s)] | s <- linSpaced n1 s_l s_u]
        areas = zipWith (zipWith (><)) dus dvs
        dus = zipWith (zipWith displacement) pts (tail pts)
        dvs = map (\row -> zipWith displacement row (tail row)) pts
        vals = map (map vf) pts
        halfAveVals = map (\row -> zipWith ave (tail row) row) vals
        aveVals = zipWith (zipWith ave) (tail halfAveVals) halfAveVals

{-
evalSquare :: (InnerSpace v, Scalar v ~ Double) => Double -> Int -> Int
             -> (Vec -> v) -> Surface
             -> Vec -> Vec -> Vec -> Vec
             -> v -> v -> v -> v -> v
evalSquare tol level maxlevel field (Surface f s_l s_u t_l t_u)
           surfll surflu surful surfuu fieldll fieldlu fieldul fielduu val
    = let s_m = (s_l + s_u) / 2
          t_m s = (t_l s + t_u s) / 2
          surflm = f (s_l,t_m s_l)
          surfum = f (s_u,t_m s_u)
          surfml = f (s_m,t_l s_m)
          surfmu = f (s_m,t_u s_m)
          surfmm = f (s_m,t_m s_m)
          fieldlm = field surflm
          fieldum = field surfum
          fieldml = field surfml
          fieldmu = field surfmu
          fieldmm = field surfmm
          dull = surfml ^-^ surfll
          dulu = surfmm ^-^ surflm
          duul = surful ^-^ surfml
          duuu = surfum ^-^ surfmm
          dvll = surflm ^-^ surfll
          dvlu = surflu ^-^ surflm
          dvul = surfmm ^-^ surfml
          dvuu = surfmu ^-^ surfmm
          areall = dull >< dvll
          arealu = dulu >< dvlu
          areaul = duul >< dvul
          areauu = duuu >< dvuu
          valll = average [fieldll,fieldlm,fieldml,fieldmm] <.> areall
          vallu = average [fieldlm,fieldlu,fieldmm,fieldmu] <.> arealu
          valul = average [fieldml,fieldmm,fieldul,fieldum] <.> areaul
          valuu = average [fieldmm,fieldmu,fieldum,fielduu] <.> areauu
          newval = valll ^+^ vallu ^+^ valul ^+^ valuu
      in if magnitude (newval ^-^ val) < tol then
             newval
         else
             evalSquare (tol/2) (level+1) maxlevel field (Surface f s_l s_m t_l t_m)
                        surfll surflm surfml surfmm fieldll fieldlm fieldml fieldmm valll ^+^
             evalSquare (tol/2) (level+1) maxlevel field (Surface f s_l s_m t_m t_u)
                        surflm surflu surfmm surfmu fieldlm fieldlu fieldmm fieldmu vallu ^+^
             evalSquare (tol/2) (level+1) maxlevel field (Surface f s_m s_u t_l t_m)
                        surfml surfmm surful surfum fieldml fieldmm fieldul fieldum valul ^+^
             evalSquare (tol/2) (level+1) maxlevel field (Surface f s_m s_u t_m t_u)
                        surfmm surfmu surfum surfuu fieldmm fieldmu fieldum fielduu valuu
-}

{-
dottedSurfIntegral :: Double
                   -> (Vec -> Vec) -> Surface
                   -> Double
dottedSurfIntegral tol vf (Surface f s_l s_u t_l t_u)
    = let surfll = f (s_l,t_l s_l)
          surflu = f (s_l,t_u s_l)
          surful = f (s_u,t_l s_u)
          surfuu = f (s_u,t_u s_u)
          fieldll = vf surfll
          fieldlu = vf surflu
          fieldul = vf surful
          fielduu = vf surfuu
          du = surful ^-^ surfll
          dv = surflu ^-^ surfll
          area = du >< dv
          val = average [fieldll,fieldlu,fieldul,fielduu] <.> area
      in evalSquare tol 1 2 20 vf (Surface f s_l s_u t_l t_u)
         surfll surflu surful surfuu fieldll fieldlu fieldul fielduu val

fullDottedSurfIntegral :: Double -> Int -> Int
                       -> (Vec -> Vec) -> Surface
                       -> Double
fullDottedSurfIntegral tol minlevel maxlevel vf (Surface f s_l s_u t_l t_u)
    = let surfll = f (s_l,t_l s_l)
          surflu = f (s_l,t_u s_l)
          surful = f (s_u,t_l s_u)
          surfuu = f (s_u,t_u s_u)
          fieldll = vf surfll
          fieldlu = vf surflu
          fieldul = vf surful
          fielduu = vf surfuu
          du = surful ^-^ surfll
          dv = surflu ^-^ surfll
          area = du >< dv
          val = average [fieldll,fieldlu,fieldul,fielduu] <.> area
      in evalSquare tol 1 minlevel maxlevel vf (Surface f s_l s_u t_l t_u)
         surfll surflu surful surfuu fieldll fieldlu fieldul fielduu val

evalSquare :: Double -> Int -> Int -> Int
           -> (Vec -> Vec) -> Surface
           -> Vec -> Vec -> Vec -> Vec
           -> Vec -> Vec -> Vec -> Vec -> Double -> Double
evalSquare tol level minlevel maxlevel field (Surface f s_l s_u t_l t_u)
           surfll surflu surful surfuu fieldll fieldlu fieldul fielduu val
    = let s_m = (s_l + s_u) / 2
          t_m s = (t_l s + t_u s) / 2
          surflm = f (s_l,t_m s_l)
          surfum = f (s_u,t_m s_u)
          surfml = f (s_m,t_l s_m)
          surfmu = f (s_m,t_u s_m)
          surfmm = f (s_m,t_m s_m)
          fieldlm = field surflm
          fieldum = field surfum
          fieldml = field surfml
          fieldmu = field surfmu
          fieldmm = field surfmm
          dull = surfml ^-^ surfll
          dulu = surfmm ^-^ surflm
          duul = surful ^-^ surfml
          duuu = surfum ^-^ surfmm
          dvll = surflm ^-^ surfll
          dvlu = surflu ^-^ surflm
          dvul = surfmm ^-^ surfml
          dvuu = surfmu ^-^ surfmm
          areall = dull >< dvll
          arealu = dulu >< dvlu
          areaul = duul >< dvul
          areauu = duuu >< dvuu
          valll = average [fieldll,fieldlm,fieldml,fieldmm] <.> areall
          vallu = average [fieldlm,fieldlu,fieldmm,fieldmu] <.> arealu
          valul = average [fieldml,fieldmm,fieldul,fieldum] <.> areaul
          valuu = average [fieldmm,fieldmu,fieldum,fielduu] <.> areauu
          newval = valll + vallu + valul + valuu
      in if level >= maxlevel || level >= minlevel && abs (newval - val) < tol then
             newval
         else
             evalSquare (tol/4) (level+1) minlevel maxlevel field (Surface f s_l s_m t_l t_m)
                        surfll surflm surfml surfmm fieldll fieldlm fieldml fieldmm valll +
             evalSquare (tol/4) (level+1) minlevel maxlevel field (Surface f s_l s_m t_m t_u)
                        surflm surflu surfmm surfmu fieldlm fieldlu fieldmm fieldmu vallu +
             evalSquare (tol/4) (level+1) minlevel maxlevel field (Surface f s_m s_u t_l t_m)
                        surfml surfmm surful surfum fieldml fieldmm fieldul fieldum valul +
             evalSquare (tol/4) (level+1) minlevel maxlevel field (Surface f s_m s_u t_m t_u)
                        surfmm surfmu surfum surfuu fieldmm fieldmu fieldum fielduu valuu
-}

-- n+1 points
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

{-
average :: (VectorSpace v, Scalar v ~ Double) => [v] -> v
average vs = sumV vs ^/ fromIntegral (length vs)

areaOfSurface :: Surface -> Double
areaOfSurface = surfaceIntegral 100 100 (const 1)
-}

-- | Shift a surface by a displacement.
shiftSurface :: Displacement -> Surface -> Surface
shiftSurface d (Surface f sl su tl tu)
    = Surface (shiftPosition d . f) sl su tl tu
