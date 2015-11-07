{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

-- | Algorithm 4.2 of Burden and Faires, 5th edition

module Physics.Learn.AdaptiveQuadrature
--    ( adaptiveQuad
--    )
    where

import Data.VectorSpace
    ( VectorSpace
    , InnerSpace
    , Scalar
    , (^+^)
    , (^-^)
    , (*^)
    , magnitude
    , sumV
    )

-- | Simplest, most elegant implementation.
--   Evaluates function at same spot multiple times.
adaptiveQuad :: Double              -- ^ tolerance
             -> Double              -- ^ lower limit a
             -> Double              -- ^ upper limit b
             -> (Double -> Double)  -- ^ function f
             -> Double              -- ^ definite integral
adaptiveQuad tol a b f
    = let s0 = simpson a b f
          m  = (a + b) / 2
          s1a = simpson a m f
          s1b = simpson m b f
      in if abs (s1a + s1b - s0) < 10 * tol
         then s1a + s1b
         else adaptiveQuad (tol/2) a m f + adaptiveQuad (tol/2) m b f

simpson :: Double              -- ^ lower limit a
        -> Double              -- ^ upper limit b
        -> (Double -> Double)  -- ^ function f
        -> Double              -- ^ Simpson approximation
simpson a b f = (b - a) / 6 * (f a + 4 * f ((a + b) / 2) + f b)

-- | Version of adaptiveQuad for vectors.
--   Evaluates function at same spot multiple times.
adaptiveQuadVec :: (InnerSpace v, Scalar v ~ Double) =>
                   Double         -- ^ tolerance
                -> Double         -- ^ lower limit a
                -> Double         -- ^ upper limit b
                -> (Double -> v)  -- ^ function f
                -> v              -- ^ definite integral
adaptiveQuadVec tol a b f
    = let s0 = simpsonVec a b f
          m  = (a + b) / 2
          s1a = simpsonVec a m f
          s1b = simpsonVec m b f
      in if magnitude (s1a ^+^ s1b ^-^ s0) < 10 * tol
         then s1a ^+^ s1b
         else adaptiveQuadVec (tol/2) a m f ^+^ adaptiveQuadVec (tol/2) m b f

-- | Version of simpson for vectors.
simpsonVec :: (VectorSpace v, Scalar v ~ Double) =>
              Double         -- ^ lower limit a
           -> Double         -- ^ upper limit b
           -> (Double -> v)  -- ^ function f
           -> v              -- ^ Simpson approximation
simpsonVec a b f = ((b - a) / 6) *^ (f a ^+^ 4 *^ f ((a + b) / 2) ^+^ f b)

-- | Burden and Faires, Example 2 on page 197
example2f :: Double -> Double
example2f x = (100 / x**2) * sin (10 / x)

example2integral :: Double
example2integral = adaptiveQuad 1e-4 1 3 example2f

-- *AdaptiveQuadrature> example2integral 
-- -1.426014810049443

-- | Does no function evaluations itself.
simpleSimpson :: Double              -- ^ lower limit a
              -> Double              -- ^ upper limit b
              -> Double              -- ^ value f(a)
              -> Double              -- ^ value f((a+b)/2)
              -> Double              -- ^ value f(b)
              -> Double              -- ^ Simpson approximation
simpleSimpson a b fa fm fb = (b - a) / 6 * (fa + 4 * fm + fb)

-- The workhorse of the adaptive Simpson method.
-- Called by adaptiveSimpson
adaptiveSimpsonStep :: Double              -- ^ tolerance
                    -> Double              -- ^ lower limit a
                    -> Double              -- ^ upper limit b
                    -> (Double -> Double)  -- ^ function f
                    -> Double              -- ^ value f(a)
                    -> Double              -- ^ value f((a+b)/2)
                    -> Double              -- ^ value f(b)
                    -> Double              -- ^ definite integral
adaptiveSimpsonStep tol a b f fa fm fb
    = let s0 = simpleSimpson a b fa fm fb
          m  = (a + b) / 2
          am = (a + m) / 2
          mb = (m + b) / 2
          fam = f am
          fmb = f mb
          s1a = simpleSimpson a m fa fam fm
          s1b = simpleSimpson m b fm fmb fb
      in if abs (s1a + s1b - s0) < 10 * tol
         then s1a + s1b
         else adaptiveSimpsonStep (tol/2) a m f fa fam fm + adaptiveSimpsonStep (tol/2) m b f fm fmb fb

-- | This version is more efficient in that it does not
--   repeat function evaluations.
adaptiveSimpson :: Double              -- ^ tolerance
                -> Double              -- ^ lower limit a
                -> Double              -- ^ upper limit b
                -> (Double -> Double)  -- ^ function f
                -> Double              -- ^ definite integral
adaptiveSimpson tol a b f
    = let fa = f a
          m = (a + b) / 2
          fm = f m
          fb = f b
      in adaptiveSimpsonStep tol a b f fa fm fb

-- | Does no function evaluations itself.
--   For vector functions.
simpleSimpsonVec :: (VectorSpace v, Fractional (Scalar v)) =>
                    Scalar v  -- ^ lower limit a
                 -> Scalar v  -- ^ upper limit b
                 -> v         -- ^ value f(a)
                 -> v         -- ^ value f((a+b)/2)
                 -> v         -- ^ value f(b)
                 -> v         -- ^ Simpson approximation
simpleSimpsonVec a b fa fm fb = ((b - a) / 6) *^ (fa ^+^ 4 *^ fm ^+^ fb)

------------------------------------------
-- Resource-limited adaptive quadrature --
------------------------------------------

{-
Want a version that gives an error estimate, and can be used by
a scheduler for a resource-limited adaptive algorithm.
We won't achieve a desired precision, but rather we'll use
a fixed amount of resources in the best way possible.

I think we'll need to create a data structure to hold the results
of evaluations so far so that they can be fed to the next step
if necessary.

-- | This version does not repeat function evaluations.
--   It provides an error estimate.


-}

-- data EvPair v = EvPair (Scalar v) v

data SimpInterval3 v = SI3 { prLo    :: (Scalar v, v)
                           , prMi    :: (Scalar v, v)
                           , prHi    :: (Scalar v, v)
                           , intEst3 :: v
                           }

data SimpInterval5 v = SI5 { pr0       :: (Scalar v, v)
                           , pr1       :: (Scalar v, v)
                           , pr2       :: (Scalar v, v)
                           , pr3       :: (Scalar v, v)
                           , pr4       :: (Scalar v, v)
                           , intEst012 :: v
                           , intEst234 :: v
                           , intEst024 :: v
                           , integralEst :: v  -- sum of intEst012 and intEst234
                           , errorEst  :: Scalar v
                           }

divideInterval :: SimpInterval5 v -> (SimpInterval3 v, SimpInterval3 v)
divideInterval (SI5 xy0 xy1 xy2 xy3 xy4 ie012 ie234 _ie024 _ _)
    = (SI3 xy0 xy1 xy2 ie012, SI3 xy2 xy3 xy4 ie234)

refineInterval :: (InnerSpace v , Floating (Scalar v)) =>
                  (Scalar v -> v)
               -> SimpInterval3 v
               -> SimpInterval5 v
refineInterval f (SI3 (x0,y0) (x2,y2) (x4,y4) ie024)
    = let x1 = (x0 + x2) / 2
          x3 = (x2 + x4) / 2
          y1 = f x1
          y3 = f x3
          ie012 = simpleSimpsonVec x0 x2 y0 y1 y2
          ie234 = simpleSimpsonVec x2 x4 y2 y3 y4
          ie = ie012 ^+^ ie234
          errEst = 1/10 * magnitude (ie ^-^ ie024)  -- 1/10 instead of 1/15
      in SI5 (x0,y0) (x1,y1) (x2,y2) (x3,y3) (x4,y4) ie012 ie234 ie024 ie errEst

divideWorstInterval :: (InnerSpace v, Ord (Scalar v), Floating (Scalar v)) =>
                       (Scalar v -> v)
                    -> [SimpInterval5 v]
                    -> [SimpInterval5 v]
divideWorstInterval _ [] = error "divideWorstInterval should never have been called on an empty list"
divideWorstInterval f (si:sis)
    = let (si3a,si3b) = divideInterval si
          si5a = refineInterval f si3a
          si5b = refineInterval f si3b
      in insertSorted si5a $ insertSorted si5b sis

insertSorted :: Ord (Scalar v) =>
                SimpInterval5 v
             -> [SimpInterval5 v]
             -> [SimpInterval5 v]
insertSorted si5 [] = [si5]
insertSorted si5 (si:sis) = if errorEst si5 > errorEst si
                            then si5:si:sis
                            else si:insertSorted si5 sis

adaptiveSimpEvalLimit :: (InnerSpace v, Ord (Scalar v), Floating (Scalar v)) =>
                         Int              -- ^ approximate number of function evals
                      -> Scalar v         -- ^ lower limit
                      -> Scalar v         -- ^ upper limit
                      -> (Scalar v -> v)  -- ^ scalar or vector function
                      -> v                -- ^ approximate integral
adaptiveSimpEvalLimit n a b f
    = let m = (a + b) / 2
          fa = f a
          fm = f m
          fb = f b
          ie = simpleSimpsonVec a b fa fm fb
          si3 = SI3 (a,fa) (m,fm) (b,fb) ie
          si5 = refineInterval f si3
      in sumV $ map integralEst $ last $ take (div n 4) $ iterate (divideWorstInterval f) [si5]

{-
data SimpsonInterval5 v = SI5 { pLo         :: Scalar v
                              , pHi         :: Scalar v
                              , fLo         :: v
                              , fLM         :: v
                              , fM          :: v
                              , fMH         :: v
                              , fHi         :: v
                              , integralEst :: v
                              , errorEst    :: Scalar v
                              }
-}

-------------------------------
-- Two-Dimensional integrals --
-------------------------------

adaptiveQuad2D :: Double              -- ^ tolerance
               -> Double              -- ^ lower limit x_0
               -> Double              -- ^ upper limit x_1
               -> (Double -> Double)  -- ^ lower limit y_0(x)
               -> (Double -> Double)  -- ^ upper limit y_1(x)
               -> (Double -> Double -> Double)  -- ^ function f
               -> Double              -- ^ definite integral
adaptiveQuad2D tol x0 x1 y0 y1 f
    = let f1 x = adaptiveQuad tol' (y0 x) (y1 x) (f x)
          tol' = tol / abs (x1 - x0)
      in adaptiveQuad tol x0 x1 f1

aq2dTest :: Double -> Double
aq2dTest tol = adaptiveQuad2D tol (-1) 1 (\y -> -sqrt(1 - y**2)) (\y -> sqrt(1-y**2)) (\_ _ -> 1)

adaptiveSimpson2D :: Double              -- ^ tolerance
                  -> Double              -- ^ lower limit x_0
                  -> Double              -- ^ upper limit x_1
                  -> (Double -> Double)  -- ^ lower limit y_0(x)
                  -> (Double -> Double)  -- ^ upper limit y_1(x)
                  -> (Double -> Double -> Double)  -- ^ function f
                  -> Double              -- ^ definite integral
adaptiveSimpson2D tol x0 x1 y0 y1 f
    = let f1 x = adaptiveSimpson tol' (y0 x) (y1 x) (f x)
          tol' = tol / abs (x1 - x0)
      in adaptiveSimpson tol x0 x1 f1

adaptiveSimpson3D :: Double              -- ^ tolerance
                  -> Double              -- ^ lower limit x_0
                  -> Double              -- ^ upper limit x_1
                  -> (Double -> Double)  -- ^ lower limit y_0(x)
                  -> (Double -> Double)  -- ^ upper limit y_1(x)
                  -> (Double -> Double -> Double)  -- ^ lower limit z_0(x,y)
                  -> (Double -> Double -> Double)  -- ^ upper limit z_1(x,y)
                  -> (Double -> Double -> Double -> Double)  -- ^ function f
                  -> Double              -- ^ definite integral
adaptiveSimpson3D tol x0 x1 y0 y1 z0 z1 f
    = let f1 x = adaptiveSimpson2D tol' (y0 x) (y1 x) (z0 x) (z1 x) (f x)
          tol' = tol / abs (x1 - x0)
      in adaptiveSimpson tol x0 x1 f1

as3dTest :: Double -> Double
as3dTest tol = adaptiveSimpson3D tol (-1) 1
               (\y -> -sqrt(1 - y**2)) (\y -> sqrt(1-y**2))
               (\x y -> -sqrt(1 - x**2 - y**2)) (\x y -> sqrt(1 - x**2 - y**2))
               (\_ _ _ -> 1)

