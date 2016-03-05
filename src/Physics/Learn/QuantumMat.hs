{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Trustworthy #-}

{- | 
Module      :  Physics.Learn.QuantumMat
Copyright   :  (c) Scott N. Walck 2016
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

This module contains state vectors and matrices
for quantum mechanics.
-}

-- Using only Complex Double here, no cyclotomic

module Physics.Learn.QuantumMat
    (
    -- * State Vectors
      xp
    , xm
    , yp
    , ym
    , zp
    , zm
    , np
    , nm
    , normalize
    -- * Matrices (operators)
    , sx
    , sy
    , sz
    -- * Quantum Dynamics
    , timeEv
    , timeEvMat
    ) where

import Numeric.LinearAlgebra
    ( C
    , Vector
    , Matrix
    , iC    -- ^ sqrt (-1)
    , fromList -- ^ vector definition
    , (><)  -- ^ matrix definition
    , (<>)  -- ^ matrix product (not * !!!!)
    , (#>)  -- ^ matrix-vector product
    , ident
    , scale
    , norm_2
    , size
    , inv
    , (<\>)
    )
import Data.Complex
    ( Complex(..)
    )

-- | The state resulting from a measurement of
--   spin angular momentum in the x direction
--   on a spin-1/2 particle
--   when the result of the measurement is hbar/2.
xp :: Vector C
xp = normalize $ fromList [1, 1]

-- | The state resulting from a measurement of
--   spin angular momentum in the x direction
--   on a spin-1/2 particle
--   when the result of the measurement is -hbar/2.
xm :: Vector C
xm = normalize $ fromList [1, -1]

-- | The state resulting from a measurement of
--   spin angular momentum in the y direction
--   on a spin-1/2 particle
--   when the result of the measurement is hbar/2.
yp :: Vector C
yp = normalize $ fromList [1, iC]

-- | The state resulting from a measurement of
--   spin angular momentum in the y direction
--   on a spin-1/2 particle
--   when the result of the measurement is -hbar/2.
ym :: Vector C
ym = normalize $ fromList [1, -iC]

-- | The state resulting from a measurement of
--   spin angular momentum in the z direction
--   on a spin-1/2 particle
--   when the result of the measurement is hbar/2.
zp :: Vector C
zp = normalize $ fromList [1, 0]

-- | The state resulting from a measurement of
--   spin angular momentum in the z direction
--   on a spin-1/2 particle
--   when the result of the measurement is -hbar/2.
zm :: Vector C
zm = normalize $ fromList [0, 1]

-- | The state resulting from a measurement of
--   spin angular momentum in the direction
--   specified by spherical angles theta (polar angle)
--   and phi (azimuthal angle)
--   on a spin-1/2 particle
--   when the result of the measurement is hbar/2.
np :: Double -> Double -> Vector C
np theta phi = fromList [ cos (theta/2) :+ 0
                        , exp(0 :+ phi) * (sin (theta/2) :+ 0) ]

-- | The state resulting from a measurement of
--   spin angular momentum in the direction
--   specified by spherical angles theta (polar angle)
--   and phi (azimuthal angle)
--   on a spin-1/2 particle
--   when the result of the measurement is -hbar/2.
nm :: Double -> Double -> Vector C
nm theta phi = fromList [ sin (theta/2) :+ 0
                        , -exp(0 :+ phi) * (cos (theta/2) :+ 0) ]

-- | Return a normalized version of a given state vector.
normalize :: Vector C -> Vector C
normalize v = scale (1 / norm_2 v :+ 0) v

-- | The Pauli X matrix.
sx :: Matrix C
sx = (2><2) [ 0, 1
            , 1, 0 ]

-- | The Pauli Y matrix.
sy :: Matrix C
sy = (2><2) [  0, -iC
            , iC,   0 ]

-- | The Pauli Z matrix.
sz :: Matrix C
sz = (2><2) [ 1,  0
            , 0, -1 ]

-- | Given a time step and a Hamiltonian matrix,
--   produce a unitary time evolution matrix.
--   Unless you really need the time evolution matrix,
--   it is better to use 'timeEv', which gives the
--   same numerical results with doing an explicit
--   matrix inversion.  The function assumes hbar = 1.
timeEvMat :: Double -> Matrix C -> Matrix C
timeEvMat dt h
    = let ah = scale (0 :+ dt / 2) h
          (l,m) = size h
          n = if l == m then m else error "timeEv needs square Hamiltonian"
          identity = ident n
      in inv (identity + ah) <> (identity - ah)

-- | Given a time step and a Hamiltonian matrix,
--   advance the state vector using the Schrodinger equation.
--   This method should be faster than using 'timeEvMat'
--   since it solves a linear system rather than calculating
--   an inverse matrix.  The function assumes hbar = 1.
timeEv :: Double -> Matrix C -> Vector C -> Vector C
timeEv dt h v
    = let ah = scale (0 :+ dt / 2) h
          (l,m) = size h
          n = if l == m then m else error "timeEv needs square Hamiltonian"
          identity = ident n
      in (identity + ah) <\> ((identity - ah) #> v)
