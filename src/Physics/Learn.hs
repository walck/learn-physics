{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Trustworthy #-}

{- | 
Module      :  Physics.Learn
Copyright   :  (c) Scott N. Walck 2014
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

Functions for learning physics.
-}

module Physics.Learn
    (
    -- * Mechanics
      TheTime
    , TimeStep
    , Velocity
    -- ** Simple one-particle state
    , SimpleState
    , SimpleAccelerationFunction
    , simpleStateDeriv
    , simpleRungeKuttaStep
    -- ** One-particle state
    , St(..)
    , DSt(..)
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
    , ChargeDistribution(..)
    , totalCharge
    -- ** Current
    , Current
    , CurrentDistribution(..)
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
    , Curve(..)
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
    , Surface(..)
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
    , Volume(..)
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
    , StateSpace(..)
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

import Physics.Learn.Charge
    ( Charge
    , ChargeDistribution(..)
    , totalCharge
    , eField
    , electricFlux
    , electricPotentialFromField
    , electricPotentialFromCharge
    )
import Physics.Learn.Current
    ( Current
    , CurrentDistribution(..)
    , bField
    , magneticFlux
    )
import Physics.Learn.CarrotVec
    ( Vec
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
    )
import Physics.Learn.Position
    ( Position
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
    )
import Physics.Learn.Curve
    ( Curve(..)
    , normalizeCurve
    , concatCurves
    , concatenateCurves
    , reverseCurve
    , evalCurve
    , shiftCurve
    , straightLine
    , simpleLineIntegral
    , dottedLineIntegral
    , crossedLineIntegral
    )
import Physics.Learn.Surface
    ( Surface(..)
    , unitSphere
    , centeredSphere
    , sphere
    , northernHemisphere
    , disk
    , shiftSurface
    , surfaceIntegral
    , dottedSurfaceIntegral
    )
import Physics.Learn.Volume
    ( Volume(..)
    , unitBall
    , unitBallCartesian
    , centeredBall
    , ball
    , northernHalfBall
    , centeredCylinder
    , shiftVolume
    , volumeIntegral
    )
import Physics.Learn.Visual.VisTools
    ( v3FromVec
    , v3FromPos
    , visVec
    , oneVector
    , displayVectorField
    , curveObject
    )
import Physics.Learn.StateSpace
    ( StateSpace(..)
    , (.-^)
    , Time
    , DifferentialEquation
    , InitialValueProblem
    , EvolutionMethod
    , SolutionMethod
    , stepSolution
    , eulerMethod
    )
import Physics.Learn.RungeKutta
    ( rungeKutta4
    , integrateSystem
    )
import Physics.Learn.Mechanics
    ( TheTime
    , TimeStep
    , Velocity
    , SimpleState
    , SimpleAccelerationFunction
    , simpleStateDeriv
    , simpleRungeKuttaStep
    , St(..)
    , DSt(..)
    , OneParticleSystemState
    , OneParticleAccelerationFunction
    , oneParticleStateDeriv
    , oneParticleRungeKuttaStep
    , oneParticleRungeKuttaSolution
    , TwoParticleSystemState
    , TwoParticleAccelerationFunction
    , twoParticleStateDeriv
    , twoParticleRungeKuttaStep
    , ManyParticleSystemState
    , ManyParticleAccelerationFunction
    , manyParticleStateDeriv
    , manyParticleRungeKuttaStep
    )
import Physics.Learn.Visual.PlotTools
    ( label
    , postscript
    , psFile
    )
import Physics.Learn.Visual.GlossTools
    ( polarToCart
    , cartToPolar
    , arrow
    , thickArrow
    )
