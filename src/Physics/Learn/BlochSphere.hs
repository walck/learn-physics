{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}

{- |
Module      :  Physics.Learn.BlochSphere
Copyright   :  (c) Scott N. Walck 2016
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

This module contains functions for displaying the
state of a spin-1/2 particle or other quantum two-level
system as a point on the Bloch Sphere.
-}

module Physics.Learn.BlochSphere
    ( VisObj
    , toPos
    , ketToPos
    , staticBlochSphere
    , displayStaticState
    , animatedBlochSphere
    , simulateBlochSphere
    , simulateBlochSphereK
    , stateProp
    , statePropK
    , evolutionBlochSphere
    , evolutionBlochSphereK
    , hamRabi
    )
    where

import qualified Physics.Learn.QuantumMat as M
import qualified Physics.Learn.Ket as K
import Physics.Learn.Ket
    ( Ket
    , Operator
    , (<>)
    , dagger
    )
import Numeric.LinearAlgebra
    ( Vector
    , Matrix
    , C
    , iC
--    , (<>)  -- matrix multiplication
--    , (|>)  -- vector definition
    , (!)   -- vector element access
    , (><)  -- matrix definition
    , scale
    , size
    )
import Data.Complex
    ( Complex(..)
    , conjugate
    , realPart
    , imagPart
    )
import Physics.Learn
    ( Position
    , v3FromPos
    , cart
    )
import SpatialMath
    ( Euler(..)
    )
import Vis
    ( VisObject(..)
    , Flavour(..)
    , Options(..)
    , Camera0(..)
    , defaultOpts
    , display
    , simulate
    , blue
    , red
    )
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

{-
3 ways to specify the state of a spin-1/2 particle:
Vector C
Ket
Position  (Bloch vector)

2 ways to specify a Hamiltonian:
Matrix C
Operator

3 choices for Vis' world:
(Float, Vector C)
(Float, Ket)
(Float, Position)
-}

-- | A Vis object.
type VisObj = VisObject Double

-- | Convert a 2x1 complex state vector for a qubit
--   into Bloch (x,y,z) coordinates.
toPos :: Vector C -> Position
toPos v
    = if size v /= 2
      then error "toPos only for size 2 vectors"
      else let z1 = v ! 0
               z2 = v ! 1
           in cart (2 * realPart (conjugate z1 * z2))
                   (2 * imagPart (conjugate z1 * z2))
                   (realPart (conjugate z1 * z1 - conjugate z2 * z2))

-- | Convert a qubit ket
--   into Bloch (x,y,z) coordinates.
ketToPos :: Ket -> Position
ketToPos psi
    = if K.dim psi /= 2
      then error "ketToPos only for qubit kets"
      else let z1 = dagger K.zp <> psi
               z2 = dagger K.zm <> psi
           in cart (2 * realPart (conjugate z1 * z2))
                   (2 * imagPart (conjugate z1 * z2))
                   (realPart (conjugate z1 * z1 - conjugate z2 * z2))

-- | A static 'VisObj' for the state of a qubit.
staticBlochSphere :: Position -> VisObj
staticBlochSphere r
    = RotEulerDeg (Euler 270 0 0) $ RotEulerDeg (Euler 0 180 0) $
      VisObjects [ Sphere 1 Wireframe blue
                 , Trans (v3FromPos r) (Sphere 0.05 Solid red)
                 ]

displayStaticBlochSphere :: Position -> IO ()
displayStaticBlochSphere r
    = display myOptions (staticBlochSphere r)

-- | Display a qubit state vector as a point on the Bloch Sphere.
displayStaticState :: Vector C -> IO ()
displayStaticState = displayStaticBlochSphere . toPos

-- | Given a Bloch vector as a function of time,
--   return a 'VisObj' as a function of time.
animatedBlochSphere :: (Double -> Position) -> (Float -> VisObj)
animatedBlochSphere f
    = staticBlochSphere . f . realToFrac

-- | Given a sample rate, initial qubit state vector, and
--   state propagation function, produce a simulation.
--   The 'Float' in the state propagation function is the time
--   since the beginning of the simulation.
simulateBlochSphere :: Double -> Vector C -> (Float -> (Float,Vector C) -> (Float,Vector C)) -> IO ()
simulateBlochSphere sampleRate initial statePropFunc
    = simulate myOptions sampleRate (0,initial) (staticBlochSphere . toPos . snd) statePropFunc

-- | Given a sample rate, initial qubit state ket, and
--   state propagation function, produce a simulation.
--   The 'Float' in the state propagation function is the time
--   since the beginning of the simulation.
simulateBlochSphereK :: Double -> Ket -> (Float -> (Float,Ket) -> (Float,Ket)) -> IO ()
simulateBlochSphereK sampleRate initial statePropFuncK
    = simulate myOptions sampleRate (0,initial) (staticBlochSphere . ketToPos . snd) statePropFuncK

{-
-- | Given a sample rate, initial qubit state vector, and
--   state propagation function, produce a simulation.
--   The 'Float' in the state propagation function is the time
--   since the beginning of the simulation.
playBlochSphere :: Double -> Vector C -> (Float -> (Float,Vector C) -> (Float,Vector C)) -> IO ()
playBlochSphere sampleRate initial statePropFunc
    = play myOptions sampleRate (0,initial) (staticBlochSphere . toPos . snd) statePropFunc
-}

-- | Produce a state propagation function from a time-dependent Hamiltonian.
stateProp :: (Double -> Matrix C) -> Float -> (Float,Vector C) -> (Float,Vector C)
stateProp ham tNew (tOld,v)
    = (tNew, M.timeEv (realToFrac dt) (ham tMid) v)
      where
        dt = tNew - tOld
        tMid = realToFrac $ (tNew + tOld) / 2

-- | Produce a state propagation function from a time-dependent Hamiltonian.
statePropK :: (Double -> Operator) -> Float -> (Float,Ket) -> (Float,Ket)
statePropK ham tNew (tOld,psi)
    = (tNew, K.timeEv (realToFrac dt) (ham tMid) psi)
      where
        dt = tNew - tOld
        tMid = realToFrac $ (tNew + tOld) / 2

-- | Given an initial qubit state and a time-dependent Hamiltonian,
--   produce a visualization.
evolutionBlochSphere :: Vector C -> (Double -> Matrix C) -> IO ()
evolutionBlochSphere psi0 ham
    = simulateBlochSphere 0.01 psi0 (stateProp ham)

-- | Given an initial qubit ket and a time-dependent Hamiltonian,
--   produce a visualization.
evolutionBlochSphereK :: Ket -> (Double -> Operator) -> IO ()
evolutionBlochSphereK psi0 ham
    = simulateBlochSphereK 0.01 psi0 (statePropK ham)

myOptions :: Options
myOptions = defaultOpts {optWindowName = "Bloch Sphere"
                        ,optInitialCamera = Just (Camera0 75 20 4)}

{-
staticBz1 :: IO ()
staticBz1 = evolutionBlochSphere M.xp (const (scale 0.9 M.sz))

staticBz2 :: IO ()
staticBz2 = evolutionBlochSphere ((2|>) [(cos (pi / 8)), (sin (pi / 8))]) (const M.sz)

staticBy1 :: IO ()
staticBy1 = evolutionBlochSphere M.xp (const M.sy)
-}

-- | Hamiltonian for nuclear magnetic resonance.
--   Explain omega0, omegaR, omega.
hamRabi :: Double ->  Double ->  Double ->  Double -> Matrix C
hamRabi omega0 omegaR omega t
    = let h11 = omega0 :+ 0
          h12 = (omegaR :+ 0) * exp (-iC * ((omega * t) :+ 0))
      in scale (1/2) $ (2><2) [h11, h12, (conjugate h12), (-h11)]

-- need to scale time

-- a pi pulse
