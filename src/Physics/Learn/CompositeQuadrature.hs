{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}

{- | 
Module      :  Physics.Learn.CompositeQuadrature
Copyright   :  (c) Scott N. Walck 2012-2014
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

Composite Trapezoid Rule and Composite Simpson's Rule
-}

module Physics.Learn.CompositeQuadrature
    ( compositeTrapezoid
    , compositeSimpson
    )
    where

import Data.VectorSpace
    ( VectorSpace
    , Scalar
    , (^+^)
    , (*^)
    , zeroV
    )

-- | Composite Trapezoid Rule
compositeTrapezoid :: (VectorSpace v, Fractional (Scalar v)) =>
                      Int -- ^ number of intervals (one less than the number of function evaluations)
                   -> Scalar v         -- ^ lower limit
                   -> Scalar v         -- ^ upper limit
                   -> (Scalar v -> v)  -- ^ function to be integrated
                   -> v                -- ^ definite integral
compositeTrapezoid n a b f
    = let dt = (b - a) / fromIntegral n
          ts = [a + fromIntegral m * dt | m <- [0..n]]
          pairs = [(t,f t) | t <- ts]
          combine [] = error "compositeSimpson: odd number of half-intervals" -- this should never happen
          combine [_] = zeroV
          combine ((t0,f0):(t1,f1):ps) = ((t1 - t0) / 2) *^ (f0 ^+^ f1) ^+^ combine ((t1,f1):ps)
      in combine pairs

-- | Composite Simpson's Rule
compositeSimpson :: (VectorSpace v, Fractional (Scalar v)) =>
                    Int -- ^ number of half-intervals (one less than the number of function evaluations)
                 -> Scalar v         -- ^ lower limit
                 -> Scalar v         -- ^ upper limit
                 -> (Scalar v -> v)  -- ^ function to be integrated
                 -> v                -- ^ definite integral
compositeSimpson n a b f
    = let nEven = 2 * div n 2
          dt = (b - a) / fromIntegral nEven
          ts = [a + fromIntegral m * dt | m <- [0..nEven]]
          pairs = [(t,f t) | t <- ts]
          combine [] = error "compositeSimpson: odd number of half-intervals" -- this should never happen
          combine [_] = zeroV
          combine (_:_:[]) = error "compositeSimpson: odd number of half-intervals" -- this should never happen
          combine ((t0,f0):(_,f1):(t2,f2):ps) = ((t2 - t0) / 6) *^ (f0 ^+^ 4 *^ f1 ^+^ f2) ^+^ combine ((t2,f2):ps)
      in combine pairs
