{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Safe #-}

{- | 
Module      :  Physics.Learn.RootFinding
Copyright   :  (c) Scott N. Walck 2012-2014
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

Functions for approximately solving equations like f(x) = 0.
These functions proceed by assuming that f is continuous,
and that a root is bracketed.  A bracket around a root consists
of numbers a, b such that f(a) f(b) <= 0.  Since the product
changes sign, there must be an x with a < x < b such that f(x) = 0.
-}

module Physics.Learn.RootFinding
    ( findRoots
    , findRootsN
    , findRoot
    , bracketRoot
    , bracketRootStep
    )
    where

-- | Given an initial bracketing of a root
--   (an interval (a,b) for which f(a) f(b) <= 0),
--   produce a bracket of arbitrary smallness.
bracketRoot :: (Ord a, Fractional a) =>
               a         -- ^ desired accuracy
            -> (a -> a)  -- ^ function
            -> (a,a)     -- ^ initial bracket
            -> (a,a)     -- ^ final bracket
bracketRoot dx f (a,b)
    = let fa = f a
          fb = f b
          bRoot ((c,fc),(d,fd)) = let m = (c + d) / 2
                                      fm = f m
                                  in if abs (c - d) <  dx
                                     then (c,d)
                                     else if fc * fm <= 0
                                          then bRoot ((c,fc),(m,fm))
                                          else bRoot ((m,fm),(d,fd))
      in if fa * fb > 0
         then error "bracketRoot:  initial interval is not a bracket"
         else bRoot ((a,fa),(b,fb))

-- | Given a bracketed root, return a half-width bracket.
bracketRootStep :: (Ord a, Fractional a) =>
                   (a -> a)       -- ^ function
                -> ((a,a),(a,a))  -- ^ original bracket
                -> ((a,a),(a,a))  -- ^ new bracket
bracketRootStep f ((a,fa),(b,fb))
    = let m = (a + b) / 2
          fm = f m
      in if fa * fm <= 0
         then ((a,fa),(m,fm))
         else ((m,fm),(b,fb))

findRootMachinePrecision :: (Double -> Double)
                         -> ((Double,Double),(Double,Double))
                         -> Double
findRootMachinePrecision f ((c,fc),(d,fd))
    = let m = (c + d) / 2
          fm = f m
      in if fc == 0
         then c
         else if fd == 0
              then d
              else if c == m
                   then c
                   else if m == d
                        then d
                        else if fc * fm <= 0
                             then findRootMachinePrecision f ((c,fc),(m,fm))
                             else findRootMachinePrecision f ((m,fm),(d,fd))

-- | Find a single root in a bracketed region.
--   The algorithm continues until it exhausts the
--   precision of a 'Double'.  This could cause the function to hang.
findRoot :: (Double -> Double)  -- ^ function
         -> (Double,Double)     -- ^ initial bracket
         -> Double              -- ^ approximate root
findRoot f (a,b)
    = let fa = f a
          fb = f b
      in if fa * fb > 0
         then error "bracketRoot:  initial interval is not a bracket"
         else findRootMachinePrecision f ((a,fa),(b,fb))

-- | Find a list of roots for a function over a given range.
--   First parameter is the initial number of intervals to
--   use to find the roots.  If roots are closely spaced,
--   this number of intervals may need to be large.
findRootsN :: Int                 -- ^ initial number of intervals to use
           -> (Double -> Double)  -- ^ function
           -> (Double,Double)     -- ^ range over which to search
           -> [Double]            -- ^ list of roots
findRootsN n f (a,b)
    = let dx = (b - a) / fromIntegral n
          xs = [a,a+dx..b]
      in map (findRootMachinePrecision f) [((x0,fx0),(x1,fx1)) | (x0,x1) <- zip xs (tail xs), let fx0 = f x0, let fx1 = f x1, fx0 * fx1 <= 0]

-- | Find a list of roots for a function over a given range.
--   There are no guarantees that all roots will be found.
--   Uses 'findRootsN' with 1000 intervals.
findRoots :: (Double -> Double)  -- ^ function
          -> (Double,Double)     -- ^ range over which to search
          -> [Double]            -- ^ list of roots
findRoots = findRootsN 1000
