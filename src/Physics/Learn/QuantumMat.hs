{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}

{- | 
Module      :  Physics.Learn.QuantumMat
Copyright   :  (c) Scott N. Walck 2016-2018
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
    , matrixFunction
    -- * Density matrices
    , couter
    , dm
    , trace
    , normalizeDM
    , oneQubitMixed
    -- * Quantum Dynamics
    , timeEvMat
    , timeEv
    , timeEvMatSpec
    -- * Composition
    , Kronecker(..)
    -- * Measurement
    , possibleOutcomes
    , outcomesProjectors
    , outcomesProbabilities
    -- * Vector and Matrix
    , Vector
    , Matrix
    )
    where

import Numeric.LinearAlgebra
    ( C
    , Vector
    , Matrix
    , Herm
    , iC        -- square root of negative one
    , (><)      -- matrix definition
    , ident
    , scale
    , norm_2
    , inv
    , (<\>)
    , sym
    , eigenvaluesSH
    , eigSH
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
    , realPart
    )
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

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

--------------
-- Matrices --
--------------

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

-- | Apply a function to a matrix.
--   Assumes the matrix is a normal matrix (a matrix
--   with an orthonormal basis of eigenvectors).
matrixFunction :: (C -> C) -> Matrix C -> Matrix C
matrixFunction f m
    = let (valv,vecm) = H.eig m
          fvalv = fromList [f val | val <- toList valv]
      in vecm <> H.diag fvalv <> tr vecm

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
--   same numerical results without doing an explicit
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

-- | Given a Hamiltonian matrix, return a function from time
--   to evolution matrix.  Uses spectral decomposition.
--   Assumes hbar = 1.
timeEvMatSpec :: Matrix C -> Double -> Matrix C
timeEvMatSpec m t = matrixFunction (\h -> exp(-iC * h * (t :+ 0))) m

-----------------
-- Composition --
-----------------

class Kronecker a where
    kron :: a -> a -> a

instance H.Product t => Kronecker (Vector t) where
    kron v1 v2 = H.fromList [c1 * c2 | c1 <- H.toList v1, c2 <- H.toList v2]

instance H.Product t => Kronecker (Matrix t) where
    kron = H.kronecker

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

-- From a Hermitian matrix, a list of pairs of eigenvalues and eigenvectors.
valsVecs :: Herm C -> [(Double,Vector C)]
valsVecs h = let (valv,m) = eigSH h
                 vals = H.toList valv
                 vecs = map (conjV . fromList) $ toLists (conjugateTranspose m)
             in zip vals vecs

-- From a Hermitian matrix, a list of pairs of eigenvalues and projectors.
valsPs :: Herm C -> [(Double,Matrix C)]
valsPs h = [(val,couter vec vec) | (val,vec) <- valsVecs h]

combineFst :: (Eq a, Num b) => [(a,b)] -> [(a,b)]
combineFst [] = []
combineFst [p] = [p]
combineFst ((x1,m1):(x2,m2):ps)
    = if x1 == x2
      then combineFst ((x1,m1+m2):ps)
      else (x1,m1):combineFst ((x2,m2):ps)

-- | Given an obervable, return a list of pairs
--   of possible outcomes and projectors
--   for each outcome.
outcomesProjectors :: Matrix C -> [(Double,Matrix C)]
outcomesProjectors m = combineFst (valsPs (sym m))

-- | Given an observable and a state vector, return a list of pairs
--   of possible outcomes and probabilites
--   for each outcome.
outcomesProbabilities :: Matrix C -> Vector C -> [(Double,Double)]
outcomesProbabilities m v
    = [(a,realPart (inner v (p #> v))) | (a,p) <- outcomesProjectors m]

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

