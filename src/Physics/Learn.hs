{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wall #-}

-- |
--Module      :  Physics.Learn
--Copyright   :  (c) Scott N. Walck 2014-2018
--License     :  BSD3 (see LICENSE)
--Maintainer  :  Scott N. Walck <walck@lvc.edu>
--Stability   :  experimental
--
--Functions for learning physics.
module Physics.Learn
  ( -- * Mechanics
    TheTime
  , TimeStep
  , Velocity

    -- ** Simple one-particle state
  , SimpleState
  , SimpleAccelerationFunction
  , simpleStateDeriv
  , simpleRungeKuttaStep

    -- ** One-particle state
  , St (..)
  , DSt (..)
  , OneParticleSystemState
  , OneParticleAccelerationFunction
  , oneParticleStateDeriv
  , oneParticleRungeKuttaStep
  , oneParticleRungeKuttaSolution

    -- ** Two-particle state
  , TwoParticleSystemState
  , TwoParticleAccelerationFunction
  , twoParticleStateDeriv
  , twoParticleRungeKuttaStep

    -- ** Many-particle state
  , ManyParticleSystemState
  , ManyParticleAccelerationFunction
  , manyParticleStateDeriv
  , manyParticleRungeKuttaStep

    -- * E&M

    -- ** Charge
  , Charge
  , ChargeDistribution (..)
  , totalCharge

    -- ** Current
  , Current
  , CurrentDistribution (..)

    -- ** Electric Field
  , eField

    -- ** Electric Flux
  , electricFlux

    -- ** Electric Potential
  , electricPotentialFromField
  , electricPotentialFromCharge

    -- ** Magnetic Field
  , bField

    -- ** Magnetic Flux
  , magneticFlux

    -- * Geometry

    -- ** Vectors
  , Vec
  , xComp
  , yComp
  , zComp
  , vec
  , (^+^)
  , (^-^)
  , (*^)
  , (^*)
  , (^/)
  , (<.>)
  , (><)
  , magnitude
  , zeroV
  , negateV
  , sumV
  , iHat
  , jHat
  , kHat

    -- ** Position
  , Position
  , Displacement
  , ScalarField
  , VectorField
  , Field
  , CoordinateSystem
  , cartesian
  , cylindrical
  , spherical
  , cart
  , cyl
  , sph
  , cartesianCoordinates
  , cylindricalCoordinates
  , sphericalCoordinates
  , displacement
  , shiftPosition
  , shiftObject
  , shiftField
  , addFields
  , rHat
  , thetaHat
  , phiHat
  , sHat
  , xHat
  , yHat
  , zHat

    -- ** Curves
  , Curve (..)
  , normalizeCurve
  , concatCurves
  , concatenateCurves
  , reverseCurve
  , evalCurve
  , shiftCurve
  , straightLine

    -- ** Line Integrals
  , simpleLineIntegral
  , dottedLineIntegral
  , crossedLineIntegral

    -- ** Surfaces
  , Surface (..)
  , unitSphere
  , centeredSphere
  , sphere
  , northernHemisphere
  , disk
  , shiftSurface

    -- ** Surface Integrals
  , surfaceIntegral
  , dottedSurfaceIntegral

    -- ** Volumes
  , Volume (..)
  , unitBall
  , unitBallCartesian
  , centeredBall
  , ball
  , northernHalfBall
  , centeredCylinder
  , shiftVolume

    -- ** Volume Integral
  , volumeIntegral

    -- * Differential Equations
  , StateSpace (..)
  , (.-^)
  , Time
  , DifferentialEquation
  , InitialValueProblem
  , EvolutionMethod
  , SolutionMethod
  , stepSolution
  , eulerMethod
  , rungeKutta4
  , integrateSystem

    -- * Visualization

    -- ** Plotting
  , label
  , postscript
  , psFile

    -- ** Gloss library
  , polarToCart
  , cartToPolar
  , arrow
  , thickArrow

    -- ** Vis library
  , v3FromVec
  , v3FromPos
  , visVec
  , oneVector
  , displayVectorField
  , curveObject
  )
where

import Physics.Learn.CarrotVec
  ( Vec
  , iHat
  , jHat
  , kHat
  , magnitude
  , negateV
  , sumV
  , vec
  , xComp
  , yComp
  , zComp
  , zeroV
  , (*^)
  , (<.>)
  , (><)
  , (^*)
  , (^+^)
  , (^-^)
  , (^/)
  )
import Physics.Learn.Charge
  ( Charge
  , ChargeDistribution (..)
  , eField
  , electricFlux
  , electricPotentialFromCharge
  , electricPotentialFromField
  , totalCharge
  )
import Physics.Learn.Current
  ( Current
  , CurrentDistribution (..)
  , bField
  , magneticFlux
  )
import Physics.Learn.Curve
  ( Curve (..)
  , concatCurves
  , concatenateCurves
  , crossedLineIntegral
  , dottedLineIntegral
  , evalCurve
  , normalizeCurve
  , reverseCurve
  , shiftCurve
  , simpleLineIntegral
  , straightLine
  )
import Physics.Learn.Mechanics
  ( DSt (..)
  , ManyParticleAccelerationFunction
  , ManyParticleSystemState
  , OneParticleAccelerationFunction
  , OneParticleSystemState
  , SimpleAccelerationFunction
  , SimpleState
  , St (..)
  , TheTime
  , TimeStep
  , TwoParticleAccelerationFunction
  , TwoParticleSystemState
  , Velocity
  , manyParticleRungeKuttaStep
  , manyParticleStateDeriv
  , oneParticleRungeKuttaSolution
  , oneParticleRungeKuttaStep
  , oneParticleStateDeriv
  , simpleRungeKuttaStep
  , simpleStateDeriv
  , twoParticleRungeKuttaStep
  , twoParticleStateDeriv
  )
import Physics.Learn.Position
  ( CoordinateSystem
  , Displacement
  , Field
  , Position
  , ScalarField
  , VectorField
  , addFields
  , cart
  , cartesian
  , cartesianCoordinates
  , cyl
  , cylindrical
  , cylindricalCoordinates
  , displacement
  , phiHat
  , rHat
  , sHat
  , shiftField
  , shiftObject
  , shiftPosition
  , sph
  , spherical
  , sphericalCoordinates
  , thetaHat
  , xHat
  , yHat
  , zHat
  )
import Physics.Learn.RungeKutta
  ( integrateSystem
  , rungeKutta4
  )
import Physics.Learn.StateSpace
  ( DifferentialEquation
  , EvolutionMethod
  , InitialValueProblem
  , SolutionMethod
  , StateSpace (..)
  , Time
  , eulerMethod
  , stepSolution
  , (.-^)
  )
import Physics.Learn.Surface
  ( Surface (..)
  , centeredSphere
  , disk
  , dottedSurfaceIntegral
  , northernHemisphere
  , shiftSurface
  , sphere
  , surfaceIntegral
  , unitSphere
  )
import Physics.Learn.Visual.GlossTools
  ( arrow
  , cartToPolar
  , polarToCart
  , thickArrow
  )
import Physics.Learn.Visual.PlotTools
  ( label
  , postscript
  , psFile
  )
import Physics.Learn.Visual.VisTools
  ( curveObject
  , displayVectorField
  , oneVector
  , v3FromPos
  , v3FromVec
  , visVec
  )
import Physics.Learn.Volume
  ( Volume (..)
  , ball
  , centeredBall
  , centeredCylinder
  , northernHalfBall
  , shiftVolume
  , unitBall
  , unitBallCartesian
  , volumeIntegral
  )
