{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}

{- | 
Module      :  Physics.Learn.RungeKutta
Copyright   :  (c) Scott N. Walck 2012-2014
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

Differential equation solving using 4th-order Runge-Kutta
-}

module Physics.Learn.RungeKutta
    ( rungeKutta4
    , integrateSystem
    )
    where

import Physics.Learn.StateSpace
    ( StateSpace(..)
    , Diff
    , Time
    , (.+^)
    )
import Data.VectorSpace
    ( (^+^)
    , (*^)
    , (^/)
    )

-- | Take a single 4th-order Runge-Kutta step
rungeKutta4 :: StateSpace p => (p -> Diff p) -> Time p -> p -> p
rungeKutta4 f dt y
    = let k0 = dt *^ f y
          k1 = dt *^ f (y .+^ k0 ^/ 2)
          k2 = dt *^ f (y .+^ k1 ^/ 2)
          k3 = dt *^ f (y .+^ k2)
      in y .+^ (k0 ^+^ 2 *^ k1 ^+^ 2 *^ k2 ^+^ k3) ^/ 6

-- | Solve a first-order system of differential equations with 4th-order Runge-Kutta
integrateSystem :: StateSpace p => (p -> Diff p) -> Time p -> p -> [p]
integrateSystem systemDerivative dt
    = iterate (rungeKutta4 systemDerivative dt)



{-
-- | Take a single 4th-order Runge-Kutta step
rungeKutta4 :: (VectorSpace v, Fractional (Scalar v)) => (v -> v) -> Scalar v -> v -> v
rungeKutta4 f h y
    = let k0 = h *^ f y
          k1 = h *^ f (y ^+^ k0 ^/ 2)
          k2 = h *^ f (y ^+^ k1 ^/ 2)
          k3 = h *^ f (y ^+^ k2)
      in y ^+^ (k0 ^+^ 2 *^ k1 ^+^ 2 *^ k2 ^+^ k3) ^/ 6

-- | Solve a first-order system of differential equations with 4th-order Runge-Kutta
integrateSystem :: (VectorSpace v, Fractional (Scalar v)) => (v -> v) -> Scalar v -> v -> [v]
integrateSystem systemDerivative dt
    = iterate (rungeKutta4 systemDerivative dt)
-}
