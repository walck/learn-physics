{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- |
--Module      :  Physics.Learn.StateSpace
--Copyright   :  (c) Scott N. Walck 2014-2019
--License     :  BSD3 (see LICENSE)
--Maintainer  :  Scott N. Walck <walck@lvc.edu>
--Stability   :  experimental
--
--A 'StateSpace' is an affine space where the associated vector space
--has scalars that are instances of 'Fractional'.
--If p is an instance of 'StateSpace', then the associated vectorspace
--'Diff' p is intended to represent the space of (time) derivatives
--of paths in p.
--
--'StateSpace' is very similar to Conal Elliott's 'AffineSpace'.
module Physics.Learn.StateSpace
  ( StateSpace (..)
  , (.-^)
  , Time
  , DifferentialEquation
  , InitialValueProblem
  , EvolutionMethod
  , SolutionMethod
  , stepSolution
  , eulerMethod
  )
where

import Data.AdditiveGroup
  ( AdditiveGroup (..)
  )
import Data.VectorSpace
  ( VectorSpace (..)
  )
import Physics.Learn.CarrotVec
  ( Vec
  , (^*)
  )
import Physics.Learn.Position
  ( Position
  , displacement
  , shiftPosition
  )

infixl 6 .+^, .-^

infix 6 .-.

-- | An instance of 'StateSpace' is a data type that can serve as the state
--   of some system.  Alternatively, a 'StateSpace' is a collection of dependent
--   variables for a differential equation.
--   A 'StateSpace' has an associated vector space for the (time) derivatives
--   of the state.  The associated vector space is a linearized version of
--   the 'StateSpace'.
class (VectorSpace (Diff p), Fractional (Scalar (Diff p))) => StateSpace p where
  -- | Associated vector space
  type Diff p

  -- | Subtract points
  (.-.) :: p -> p -> Diff p

  -- | Point plus vector
  (.+^) :: p -> Diff p -> p

-- | The scalars of the associated vector space can be thought of as time intervals.
type Time p = Scalar (Diff p)

-- | Point minus vector
(.-^) :: StateSpace p => p -> Diff p -> p
p .-^ v = p .+^ negateV v

instance StateSpace Double where
  type Diff Double = Double
  (.-.) = (-)
  (.+^) = (+)

instance StateSpace Vec where
  type Diff Vec = Vec
  (.-.) = (^-^)
  (.+^) = (^+^)

-- | Position is not a vector, but displacement (difference in position) is a vector.
instance StateSpace Position where
  type Diff Position = Vec
  (.-.) = flip displacement
  (.+^) = flip shiftPosition

instance (StateSpace p, StateSpace q, Time p ~ Time q) => StateSpace (p, q) where
  type Diff (p, q) = (Diff p, Diff q)
  (p, q) .-. (p', q') = (p .-. p', q .-. q')
  (p, q) .+^ (u, v) = (p .+^ u, q .+^ v)

instance
  ( StateSpace p
  , StateSpace q
  , StateSpace r
  , Time p ~ Time q
  , Time q ~ Time r
  )
  => StateSpace (p, q, r)
  where
  type Diff (p, q, r) = (Diff p, Diff q, Diff r)
  (p, q, r) .-. (p', q', r') = (p .-. p', q .-. q', r .-. r')
  (p, q, r) .+^ (u, v, w) = (p .+^ u, q .+^ v, r .+^ w)

inf :: a -> [a]
inf x = x : inf x

instance AdditiveGroup v => AdditiveGroup [v] where
  zeroV = inf zeroV
  (^+^) = zipWith (^+^)
  negateV = map negateV

instance VectorSpace v => VectorSpace [v] where
  type Scalar [v] = Scalar v
  c *^ xs = [c *^ x | x <- xs]

instance StateSpace p => StateSpace [p] where
  type Diff [p] = [Diff p]
  (.-.) = zipWith (.-.)
  (.+^) = zipWith (.+^)

-- | A differential equation expresses how the dependent variables (state)
--   change with the independent variable (time).
--   A differential equation is specified by giving the (time) derivative
--   of the state as a function of the state.
--   The (time) derivative of a state is an element of the associated vector space.
type DifferentialEquation state = state -> Diff state

-- | An initial value problem is a differential equation along with an initial state.
type InitialValueProblem state = (DifferentialEquation state, state)

-- | A (numerical) solution method is a way of converting
--   an initial value problem into a list of states (a solution).
--   The list of states need not be equally spaced in time.
type SolutionMethod state = InitialValueProblem state -> [state]

-- | An evolution method is a way of approximating the state
--   after advancing a finite interval in the independent
--   variable (time) from a given state.
type EvolutionMethod state =
  DifferentialEquation state
  -- ^ differential equation
  -> Time state
  -- ^ time interval
  -> state
  -- ^ initial state
  -> state
  -- ^ evolved state

-- | Given an evolution method and a time step, return the solution method
--   which applies the evolution method repeatedly with with given time step.
--   The solution method returned will produce an infinite list of states.
stepSolution :: EvolutionMethod state -> Time state -> SolutionMethod state
stepSolution ev dt (de, ic) = iterate (ev de dt) ic

-- | The Euler method is the simplest evolution method.
--   It increments the state by the derivative times the time step.
eulerMethod :: StateSpace state => EvolutionMethod state
eulerMethod de dt st = st .+^ de st ^* dt
