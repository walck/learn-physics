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
    -- * Complex numbers
      C
    -- * State Vectors
    , xp
    , xm
    , yp
    , ym
    , zp
    , zm
    , np
    , nm
    , dim
    , scaleV
    , inner
    , norm
    , normalize
    , probVector
    , gramSchmidt
    , conjV
    , fromList
    , toList
    -- * Matrices (operators)
    , sx
    , sy
    , sz
    , scaleM
    , (<>)
    , (#>)
    , (<#)
    , conjugateTranspose
    , fromLists
    , toLists
    , size
    -- * Density matrices
    , couter
    , dm
    , trace
    , normalizeDM
    , oneQubitMixed
    -- * Quantum Dynamics
    , timeEv
    , timeEvMat
    -- * Measurement
    , possibleOutcomes
    -- * Vector and Matrix
    , Vector
    , Matrix
    )
    where

import Numeric.LinearAlgebra
    ( C
    , Vector
    , Matrix
    , iC        -- square root of negative one
    , (><)      -- matrix definition
    , ident
    , scale
    , norm_2
    , inv
    , (<\>)
    , sym
    , eigenvaluesSH
    , cmap
    , takeDiag
    , conj
    , dot
    , tr
    )
--    , (<>)      -- matrix product (not * !!!!)
--    , (#>)      -- matrix-vector product
--    , fromList  -- vector definition

import qualified Numeric.LinearAlgebra as H
-- because H.outer does not conjugate
import Data.Complex
    ( Complex(..)
    , magnitude
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

-- | Dimension of a vector.
dim :: Vector C -> Int
dim = H.size

-- | Scale a complex vector by a complex number.
scaleV :: C -> Vector C -> Vector C
scaleV = scale

-- | Complex inner product.  First vector gets conjugated.
inner :: Vector C -> Vector C -> C
inner = dot

-- | Length of a complex vector.
norm :: Vector C -> Double
norm = norm_2

-- | Return a normalized version of a given state vector.
normalize :: Vector C -> Vector C
normalize v = scale (1 / norm_2 v :+ 0) v

-- | Return a vector of probabilities for a given state vector.
probVector :: Vector C       -- ^ state vector
           -> Vector Double  -- ^ vector of probabilities
probVector = cmap (\c -> magnitude c**2)

-- | Conjugate the entries of a vector.
conjV :: Vector C -> Vector C
conjV = conj

-- | Construct a vector from a list of complex numbers.
fromList :: [C] -> Vector C
fromList = H.fromList

-- | Produce a list of complex numbers from a vector.
toList :: Vector C -> [C]
toList = H.toList

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

-- | Scale a complex matrix by a complex number.
scaleM :: C -> Matrix C -> Matrix C
scaleM = scale

-- | Matrix product.
(<>) :: Matrix C -> Matrix C -> Matrix C
(<>) = (H.<>)

-- | Matrix-vector product.
(#>) :: Matrix C -> Vector C -> Vector C
(#>) = (H.#>)

-- | Vector-matrix product
(<#) :: Vector C -> Matrix C -> Vector C
(<#) = (H.<#)

-- | Conjugate transpose of a matrix.
conjugateTranspose :: Matrix C -> Matrix C
conjugateTranspose = tr

-- | Construct a matrix from a list of lists of complex numbers.
fromLists :: [[C]] -> Matrix C
fromLists = H.fromLists

-- | Produce a list of lists of complex numbers from a matrix.
toLists :: Matrix C -> [[C]]
toLists = H.toLists

-- | Size of a matrix.
size :: Matrix C -> (Int,Int)
size = H.size

----------------------
-- Density Matrices --
----------------------

-- | Complex outer product
couter :: Vector C -> Vector C -> Matrix C
couter v w = v `H.outer` conj w

-- | Build a pure-state density matrix from a state vector.
dm :: Vector C -> Matrix C
dm cvec = cvec `couter` cvec

-- | Trace of a matrix.
trace :: Matrix C -> C
trace = sum . toList . takeDiag

-- | Normalize a density matrix so that it has trace one.
normalizeDM :: Matrix C -> Matrix C
normalizeDM rho = scale (1 / trace rho) rho

-- | The one-qubit totally mixed state.
oneQubitMixed :: Matrix C
oneQubitMixed = normalizeDM $ ident 2

----------------------
-- Quantum Dynamics --
----------------------

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

-----------------
-- Measurement --
-----------------

-- | The possible outcomes of a measurement
--   of an observable.
--   These are the eigenvalues of the matrix
--   of the observable.
possibleOutcomes :: Matrix C -> [Double]
possibleOutcomes observable
    = H.toList $ eigenvaluesSH (sym observable)

------------------
-- Gram-Schmidt --
------------------

-- | Form an orthonormal list of complex vectors
--   from a linearly independent list of complex vectors.
gramSchmidt :: [Vector C] -> [Vector C]
gramSchmidt [] = []
gramSchmidt (v:vs) = let nvs = gramSchmidt vs
                         nv = normalize (v - sum [scale (inner w v) w | w <- nvs])
                     in nv:nvs

-- To Do
--   Generate higher spin operators and state vectors
--   eigenvectors
--   projection operators

