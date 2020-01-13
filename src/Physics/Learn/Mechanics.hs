{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Safe #-}

{- | 
Module      :  Physics.Learn.Mechanics
Copyright   :  (c) Scott N. Walck 2014-2019
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

Newton's second law and all that
-}

module Physics.Learn.Mechanics
    ( TheTime
    , TimeStep
    , Velocity
    -- * Simple one-particle state
    , SimpleState
    , SimpleAccelerationFunction
    , simpleStateDeriv
    , simpleRungeKuttaStep
    -- * One-particle state
    , St(..)
    , DSt(..)
    , OneParticleSystemState
    , OneParticleAccelerationFunction
    , oneParticleStateDeriv
    , oneParticleRungeKuttaStep
    , oneParticleRungeKuttaSolution
    -- * Two-particle state
    , TwoParticleSystemState
    , TwoParticleAccelerationFunction
    , twoParticleStateDeriv
    , twoParticleRungeKuttaStep
    -- * Many-particle state
    , ManyParticleSystemState
    , ManyParticleAccelerationFunction
    , manyParticleStateDeriv
    , manyParticleRungeKuttaStep
    )
    where

import Data.VectorSpace
    ( AdditiveGroup(..)
    , VectorSpace(..)
    )
import Physics.Learn.StateSpace
    ( StateSpace(..)
    , Diff
    , DifferentialEquation
    )
import Physics.Learn.RungeKutta
    ( rungeKutta4
    , integrateSystem
    )
import Physics.Learn.Position
    ( Position
    )
import Physics.Learn.CarrotVec
    ( Vec
    )

-- | Time (in s).
type TheTime = Double

-- | A time step (in s).
type TimeStep = Double

-- | Velocity of a particle (in m/s).
type Velocity = Vec

-------------------------------
-- Simple one-particle state --
-------------------------------

-- | A simple one-particle state,
--   to get started quickly with mechanics of one particle.
type SimpleState = (TheTime,Position,Velocity)

-- | An acceleration function gives the particle's acceleration as
--   a function of the particle's state.
--   The specification of this function is what makes one single-particle
--   mechanics problem different from another.
--   In order to write this function, add all of the forces
--   that act on the particle, and divide this net force by the particle's mass.
--   (Newton's second law).
type SimpleAccelerationFunction = SimpleState -> Vec

-- | Time derivative of state for a single particle
--   with a constant mass.
simpleStateDeriv :: SimpleAccelerationFunction  -- ^ acceleration function for the particle
                 -> DifferentialEquation SimpleState  -- ^ differential equation
simpleStateDeriv a (t, r, v) = (1, v, a(t, r, v))

-- | Single Runge-Kutta step
simpleRungeKuttaStep :: SimpleAccelerationFunction  -- ^ acceleration function for the particle
                     -> TimeStep  -- ^ time step
                     -> SimpleState  -- ^ initial state
                     -> SimpleState  -- ^ state after one time step
simpleRungeKuttaStep = rungeKutta4 . simpleStateDeriv

------------------------
-- One-particle state --
------------------------

-- | The state of a single particle is given by
--   the position of the particle and the velocity of the particle.
data St = St { position :: Position
             , velocity :: Velocity
             }
          deriving (Show)

-- | The associated vector space for the
--   state of a single particle.
data DSt = DSt Vec Vec
           deriving (Show)

instance AdditiveGroup DSt where
    zeroV = DSt zeroV zeroV
    negateV (DSt dr dv) = DSt (negateV dr) (negateV dv)
    DSt dr1 dv1 ^+^ DSt dr2 dv2 = DSt (dr1 ^+^ dr2) (dv1 ^+^ dv2)

instance VectorSpace DSt where
    type Scalar DSt = Double
    c *^ DSt dr dv = DSt (c*^dr) (c*^dv)

instance StateSpace St where
    type Diff St = DSt
    St r1 v1 .-. St r2 v2  = DSt (r1 .-. r2) (v1 .-. v2)
    St r1 v1 .+^ DSt dr dv = St (r1 .+^ dr) (v1 .+^ dv)

-- | The state of a system of one particle is given by the current time,
--   the position of the particle, and the velocity of the particle.
--   Including time in the state like this allows us to
--   have time-dependent forces.
type OneParticleSystemState = (TheTime,St)

-- | An acceleration function gives the particle's acceleration as
--   a function of the particle's state.
type OneParticleAccelerationFunction = OneParticleSystemState -> Vec

-- | Time derivative of state for a single particle
--   with a constant mass.
oneParticleStateDeriv :: OneParticleAccelerationFunction  -- ^ acceleration function for the particle
                      -> DifferentialEquation OneParticleSystemState  -- ^ differential equation
oneParticleStateDeriv a st@(_t, St _r v) = (1, DSt v (a st))

-- | Single Runge-Kutta step
oneParticleRungeKuttaStep :: OneParticleAccelerationFunction  -- ^ acceleration function for the particle
                          -> TimeStep  -- ^ time step
                          -> OneParticleSystemState  -- ^ initial state
                          -> OneParticleSystemState  -- ^ state after one time step
oneParticleRungeKuttaStep = rungeKutta4 . oneParticleStateDeriv

-- | List of system states
oneParticleRungeKuttaSolution :: OneParticleAccelerationFunction  -- ^ acceleration function for the particle
                              -> TimeStep  -- ^ time step
                              -> OneParticleSystemState  -- ^ initial state
                              -> [OneParticleSystemState]  -- ^ state after one time step
oneParticleRungeKuttaSolution = integrateSystem . oneParticleStateDeriv

------------------------
-- Two-particle state --
------------------------

-- | The state of a system of two particles is given by the current time,
--   the position and velocity of particle 1,
--   and the position and velocity of particle 2.
type TwoParticleSystemState = (TheTime,St,St)

-- | An acceleration function gives a pair of accelerations
--   (one for particle 1, one for particle 2) as
--   a function of the system's state.
type TwoParticleAccelerationFunction = TwoParticleSystemState -> (Vec,Vec)

-- | Time derivative of state for two particles
--   with constant mass.
twoParticleStateDeriv :: TwoParticleAccelerationFunction  -- ^ acceleration function for two particles
                      -> DifferentialEquation TwoParticleSystemState  -- ^ differential equation
twoParticleStateDeriv af2 st2@(_t, St _r1 v1, St _r2 v2) = (1, DSt v1 a1, DSt v2 a2)
    where
      (a1,a2) = af2 st2

-- | Single Runge-Kutta step for two-particle system
twoParticleRungeKuttaStep :: TwoParticleAccelerationFunction  -- ^ acceleration function
                          -> TimeStep  -- ^ time step
                          -> TwoParticleSystemState  -- ^ initial state
                          -> TwoParticleSystemState  -- ^ state after one time step
twoParticleRungeKuttaStep = rungeKutta4 . twoParticleStateDeriv

-------------------------
-- Many-particle state --
-------------------------

-- | The state of a system of many particles is given by the current time
--   and a list of one-particle states.
type ManyParticleSystemState = (TheTime,[St])

-- | An acceleration function gives a list of accelerations
--   (one for each particle) as
--   a function of the system's state.
type ManyParticleAccelerationFunction = ManyParticleSystemState -> [Vec]

-- | Time derivative of state for many particles
--   with constant mass.
manyParticleStateDeriv :: ManyParticleAccelerationFunction  -- ^ acceleration function for many particles
                       -> DifferentialEquation ManyParticleSystemState  -- ^ differential equation
manyParticleStateDeriv af st@(_t, sts) = (1, [DSt v a | (v,a) <- zip vs as])
    where
      vs = map velocity sts
      as = af st

-- | Single Runge-Kutta step for many-particle system
manyParticleRungeKuttaStep :: ManyParticleAccelerationFunction  -- ^ acceleration function
                           -> TimeStep  -- ^ time step
                           -> ManyParticleSystemState  -- ^ initial state
                           -> ManyParticleSystemState  -- ^ state after one time step
manyParticleRungeKuttaStep = rungeKutta4 . manyParticleStateDeriv



-- Can we automatically incorporate Newton's third law?

