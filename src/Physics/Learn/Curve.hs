{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Trustworthy #-}

{- | 
Module      :  Physics.Learn.Curve
Copyright   :  (c) Scott N. Walck 2012-2014
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

This module contains functions for working with 'Curve's
and line integrals along 'Curve's.
-}

module Physics.Learn.Curve
    (
    -- * Curves
      Curve(..)
    , normalizeCurve
    , concatCurves
    , concatenateCurves
    , reverseCurve
    , evalCurve
    , shiftCurve
    , straightLine
    -- * Line Integrals
    , simpleLineIntegral
    , dottedLineIntegral
    , crossedLineIntegral
    , compositeTrapezoidDottedLineIntegral
    , compositeTrapezoidCrossedLineIntegral
    , compositeSimpsonDottedLineIntegral
    , compositeSimpsonCrossedLineIntegral
    )
    where

import Data.VectorSpace
    ( VectorSpace
    , InnerSpace
    , Scalar
    )
import Physics.Learn.CarrotVec
    ( Vec
    , (><)
    , (<.>)
    , sumV
    , (^*)
    , (^/)
    , (^+^)
    , (^-^)
    , (*^)
    , magnitude
    , zeroV
    , negateV
    )
import Physics.Learn.Position
    ( Position
    , Displacement
    , displacement
    , Field
    , VectorField
    , shiftPosition
    )

-- | 'Curve' is a parametrized function into three-space, an initial limit, and a final limit.
data Curve = Curve { curveFunc          :: Double -> Position  -- ^ function from one parameter into space
                   , startingCurveParam :: Double              -- ^ starting value of the parameter
                   , endingCurveParam   :: Double              -- ^ ending value of the parameter
                   }

-- | A dotted line integral.
--   Convenience function for 'compositeSimpsonDottedLineIntegral'.
dottedLineIntegral
    :: Int          -- ^ number of half-intervals
                    --   (one less than the number of function evaluations)
    -> VectorField  -- ^ vector field
    -> Curve        -- ^ curve to integrate over
    -> Double       -- ^ scalar result
dottedLineIntegral = compositeSimpsonDottedLineIntegral

-- | Calculates integral vf x dl over curve.
--   Convenience function for 'compositeSimpsonCrossedLineIntegral'.
crossedLineIntegral
    :: Int          -- ^ number of half-intervals
                    --   (one less than the number of function evaluations)
    -> VectorField  -- ^ vector field
    -> Curve        -- ^ curve to integrate over
    -> Vec          -- ^ vector result
crossedLineIntegral = compositeSimpsonCrossedLineIntegral

-- | A dotted line integral, performed in an unsophisticated way.
compositeTrapezoidDottedLineIntegral
    :: Int          -- ^ number of intervals
    -> VectorField  -- ^ vector field
    -> Curve        -- ^ curve to integrate over
    -> Double       -- ^ scalar result
compositeTrapezoidDottedLineIntegral n vf (Curve f a b)
    = sum $ zipWith (<.>) aveVecs dls
      where
        dt = (b - a) / fromIntegral n
        pts = [f t | t <- [a,a+dt..b]]
        vecs = [vf pt | pt <- pts]
        aveVecs = zipWith average vecs (tail vecs)
        dls = zipWith displacement pts (tail pts)

-- | Calculates integral vf x dl over curve in an unsophisticated way.
compositeTrapezoidCrossedLineIntegral
    :: Int          -- ^ number of intervals
    -> VectorField  -- ^ vector field
    -> Curve        -- ^ curve to integrate over
    -> Vec          -- ^ vector result
compositeTrapezoidCrossedLineIntegral n vf (Curve f a b)
    = sumV $ zipWith (><) aveVecs dls
      where
        dt = (b - a) / fromIntegral n
        pts = [f t | t <- [a,a+dt..b]]
        vecs = [vf pt | pt <- pts]
        aveVecs = zipWith average vecs (tail vecs)
        dls = zipWith displacement pts (tail pts)

-- | Calculates integral f dl over curve, where dl is a scalar line element.
simpleLineIntegral
    :: (InnerSpace v, Scalar v ~ Double)
       => Int      -- ^ number of intervals
    -> Field v     -- ^ scalar or vector field
    -> Curve       -- ^ curve to integrate over
    -> v           -- ^ scalar or vector result
simpleLineIntegral n vf (Curve f a b)
    = sumV $ zipWith (^*) aveVecs (map magnitude dls)
      where
        dt = (b - a) / fromIntegral n
        pts = [f t | t <- [a,a+dt..b]]
        vecs = [vf pt | pt <- pts]
        aveVecs = zipWith average vecs (tail vecs)
        dls = zipWith displacement pts (tail pts)

-- | Reparametrize a curve from 0 to 1.
normalizeCurve :: Curve -> Curve
normalizeCurve (Curve f a b)
    = Curve (f . scl) 0 1
      where
        scl t = a + (b - a) * t

-- | Concatenate two curves.
concatCurves :: Curve  -- ^ go first along this curve
             -> Curve  -- ^ then along this curve
             -> Curve  -- ^ to produce this new curve
concatCurves c1 c2
    = normalizeCurve $ Curve f 0 2
      where
        (Curve f1 _ _) = normalizeCurve c1
        (Curve f2 _ _) = normalizeCurve c2
        f t | t <= 1     = f1 t
            | otherwise  = f2 (t-1)

-- | Concatenate a list of curves.
--   Parametrizes curves equally.
concatenateCurves :: [Curve] -> Curve
concatenateCurves []     = error "concatenateCurves:  cannot concatenate empty list"
concatenateCurves cs = normalizeCurve $ Curve f 0 (fromIntegral n)
    where
      n   = length cs
      ncs = map normalizeCurve cs
      f t = evalCurve (ncs !! m) (t - fromIntegral m)
          where m = min (n-1) (floor t)

-- | Reverse a curve.
reverseCurve :: Curve -> Curve
reverseCurve (Curve f a b)
    = Curve (f . rev) a b
      where
        rev t = a + b - t

-- | Evaluate the position of a curve at a parameter.
evalCurve :: Curve     -- ^ the curve
          -> Double    -- ^ the parameter
          -> Position  -- ^ position of the point on the curve at that parameter
evalCurve (Curve f _ _) t = f t

-- | Shift a curve by a displacement.
shiftCurve :: Displacement  -- ^ amount to shift
           -> Curve         -- ^ original curve
           -> Curve         -- ^ shifted curve
shiftCurve d (Curve f sl su)
    = Curve (shiftPosition d . f) sl su

-- | The straight-line curve from one position to another.
straightLine :: Position  -- ^ starting position
             -> Position  -- ^ ending position
             -> Curve     -- ^ straight-line curve
straightLine r1 r2 = Curve f 0 1
    where
      f t = shiftPosition (t *^ d) r1
      d = displacement r1 r2

-------------
-- Helpers --
-------------

average :: (VectorSpace v, Scalar v ~ Double) => v -> v -> v
average v1 v2 = (v1 ^+^ v2) ^/ 2

----------------------------------------
-- Quadratic (Simpson) Approximations --
----------------------------------------

dottedSimp :: (InnerSpace v, Fractional (Scalar v)) =>
              v  -- ^ vector field low
           -> v  -- ^ vector field mid
           -> v  -- ^ vector field high
           -> v  -- ^ dl low to mid
           -> v  -- ^ dl mid to high
           -> Scalar v  -- ^ quadratic approximation
dottedSimp f0 f1 f2 g10 g21
    = ((g21 ^+^ g10) ^/ 6) <.> (f0 ^+^ 4 *^ f1 ^+^ f2)
      + ((g21 ^-^ g10) ^/ 3) <.> (f2 ^-^ f0)

-- | Quadratic approximation to vector field.
--   Quadratic approximation to curve.
--   Composite strategy.
--   Dotted line integral.
compositeSimpsonDottedLineIntegral
    :: Int          -- ^ number of half-intervals
                    --   (one less than the number of function evaluations)
    -> VectorField  -- ^ vector field
    -> Curve        -- ^ curve to integrate over
    -> Double       -- ^ scalar result
compositeSimpsonDottedLineIntegral n vf (Curve c a b)
    = let nEven = 2 * div n 2
          dt = (b - a) / fromIntegral nEven
          ts = [a + fromIntegral m * dt | m <- [0..nEven]]
          pairs = [(ct,vf ct) | t <- ts, let ct = c t]
          combine [] = error "compositeSimpson: odd number of half-intervals" -- this should never happen
          combine [_] = zeroV
          combine (_:_:[]) = error "compositeSimpson: odd number of half-intervals" -- this should never happen
          combine ((c0,f0):(c1,f1):(c2,f2):ps)
              = dottedSimp f0 f1 f2 (displacement c0 c1) (displacement c1 c2)
                ^+^ combine ((c2,f2):ps)
      in combine pairs

crossedSimp :: Vec  -- ^ vector field low
            -> Vec  -- ^ vector field mid
            -> Vec  -- ^ vector field high
            -> Vec  -- ^ dl low to mid
            -> Vec  -- ^ dl mid to high
            -> Vec  -- ^ quadratic approximation
crossedSimp f0 f1 f2 g10 g21
    = negateV $
      ((g21 ^+^ g10) ^/ 6) >< (f0 ^+^ 4 *^ f1 ^+^ f2)
      ^+^ ((g21 ^-^ g10) ^/ 3) >< (f2 ^-^ f0)

-- | Quadratic approximation to vector field.
--   Quadratic approximation to curve.
--   Composite strategy.
--   Crossed line integral.
compositeSimpsonCrossedLineIntegral
    :: Int          -- ^ number of half-intervals
                    --   (one less than the number of function evaluations)
    -> VectorField  -- ^ vector field
    -> Curve        -- ^ curve to integrate over
    -> Vec          -- ^ vector result
compositeSimpsonCrossedLineIntegral n vf (Curve c a b)
    = let nEven = 2 * div n 2
          dt = (b - a) / fromIntegral nEven
          ts = [a + fromIntegral m * dt | m <- [0..nEven]]
          pairs = [(ct,vf ct) | t <- ts, let ct = c t]
          combine [] = error "compositeSimpson: odd number of half-intervals" -- this should never happen
          combine [_] = zeroV
          combine (_:_:[]) = error "compositeSimpson: odd number of half-intervals" -- this should never happen
          combine ((c0,f0):(c1,f1):(c2,f2):ps)
              = crossedSimp f0 f1 f2 (displacement c0 c1) (displacement c1 c2)
                ^+^ combine ((c2,f2):ps)
      in combine pairs

