{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE CPP #-}

{- | 
Module      :  Physics.Learn.Ket
Copyright   :  (c) Scott N. Walck 2016-2018
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

This module contains ket vectors, bra vectors,
and operators for quantum mechanics.
-}

-- a Ket layer on top of QuantumMat

module Physics.Learn.Ket
    (
    -- * Basic data types
      C
    , i
    , magnitude
    , Ket
    , Bra
    , Operator
    -- * Kets for spin-1/2 particles
    , xp
    , xm
    , yp
    , ym
    , zp
    , zm
    , np
    , nm
    -- * Operators for spin-1/2 particles
    , sx
    , sy
    , sz
    , sn
    , sn'
    -- * Quantum Dynamics
    , timeEvOp
    , timeEv
    -- * Composition
    , Kron(..)
    -- * Measurement
    , possibleOutcomes
    , outcomesProjectors
    , outcomesProbabilities
--    , prob
--    , probs
    -- * Generic multiplication
    , Mult(..)
    -- * Adjoint operation
    , Dagger(..)
    -- * Normalization
    , HasNorm(..)
    -- * Representation
    , Representable(..)
    -- * Orthonormal bases
    , OrthonormalBasis
    , makeOB
    , listBasis
    , size
    -- * Orthonormal bases for spin-1/2 particles
    , xBasis
    , yBasis
    , zBasis
    , nBasis
    -- , angularMomentumXMatrix
    -- , angularMomentumYMatrix
    -- , angularMomentumZMatrix
    -- , angularMomentumPlusMatrix
    -- , angularMomentumMinusMatrix
    -- , jXMatrix
    -- , jYMatrix
    -- , jZMatrix
    -- , matrixCommutator
    -- , rotationMatrix
    -- , jmColumn
    )
    where

-- We try to import only from QuantumMat
-- and not from Numeric.LinearAlgebra

import qualified Data.Complex as C
import Data.Complex
    ( Complex(..)
    , conjugate
    )
import qualified Physics.Learn.QuantumMat as M
import Physics.Learn.QuantumMat
    ( C
    , Vector
    , Matrix
    , (#>)
    , (<#)
    , conjugateTranspose
    , scaleV
    , scaleM
    , conjV
    , fromList
    , toList
    , fromLists
    )
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

infixl 7 <>

-- | A ket vector describes the state of a quantum system.
data Ket = Ket (Vector C)

instance Show Ket where
    show k =
        let message = "Use 'rep <basis name> <ket name>'."
        in if dim k == 2
           then "Representation in zBasis:\n" ++
                show (rep zBasis k) ++ "\n" ++ message
           else message

-- | An operator describes an observable (a Hermitian operator)
--   or an action (a unitary operator).
data Operator = Operator (Matrix C)

instance Show Operator where
    show op =
        let message = "Use 'rep <basis name> <operator name>'."
        in if dim op == 2
           then "Representation in zBasis:\n" ++
                show (rep zBasis op) ++ "\n" ++ message
           else message

-- | A bra vector describes the state of a quantum system.
data Bra = Bra (Vector C)

instance Show Bra where
    show _ = "<bra>\nTry 'rep zBasis <bra name>'"

magnitude :: C -> Double
magnitude = C.magnitude

i :: C
i = 0 :+ 1

-- | Generic multiplication including inner product,
--   outer product, operator product, and whatever else makes sense.
--   No conjugation takes place in this operation.
class Mult a b c | a b -> c where
    (<>) :: a -> b -> c

instance Mult C C C where
    z1 <> z2 = z1 * z2

instance Mult C Ket Ket where
    c <> Ket matrixKet = Ket (scaleV c matrixKet)

instance Mult C Bra Bra where
    c <> Bra matrixBra = Bra (scaleV c matrixBra)

instance Mult C Operator Operator where
    c <> Operator m = Operator (scaleM c m)

instance Mult Ket C Ket where
    Ket matrixKet <> c = Ket (scaleV c matrixKet)

instance Mult Bra C Bra where
    Bra matrixBra <> c = Bra (scaleV c matrixBra)

instance Mult Operator C Operator where
    Operator m <> c = Operator (scaleM c m)

instance Mult Bra Ket C where
    Bra matrixBra <> Ket matrixKet
        = sum $ zipWith (*) (toList matrixBra) (toList matrixKet)

instance Mult Bra Operator Bra where
    Bra matrixBra <> Operator matrixOp
        = Bra (matrixBra <# matrixOp)

instance Mult Operator Ket Ket where
    Operator matrixOp <> Ket matrixKet
        = Ket (matrixOp #> matrixKet)

instance Mult Ket Bra Operator where
    Ket k <> Bra b =
        Operator
        (fromLists [[ x*y | y <- toList b] | x <- toList k])

instance Mult Operator Operator Operator where
    Operator m1 <> Operator m2 = Operator (m1 M.<> m2)

instance Num Ket where
    Ket v1 + Ket v2 = Ket (v1 + v2)
    Ket v1 - Ket v2 = Ket (v1 - v2)
    (*)         = error "Multiplication is not defined for kets"
    negate (Ket v) = Ket (negate v)
    abs         = error "abs is not defined for kets"
    signum      = error "signum is not defined for kets"
    fromInteger = error "fromInteger is not defined for kets"

instance Num Bra where
    Bra v1 + Bra v2 = Bra (v1 + v2)
    Bra v1 - Bra v2 = Bra (v1 - v2)
    (*)         = error "Multiplication is not defined for bra vectors"
    negate (Bra v) = Bra (negate v)
    abs         = error "abs is not defined for bra vectors"
    signum      = error "signum is not defined for bra vectors"
    fromInteger = error "fromInteger is not defined for bra vectors"

instance Num Operator where
    Operator v1 + Operator v2 = Operator (v1 + v2)
    Operator v1 - Operator v2 = Operator (v1 - v2)
    Operator v1 * Operator v2 = Operator (v1 M.<> v2)
    negate (Operator v) = Operator (negate v)
    abs         = error "abs is not defined for operators"
    signum      = error "signum is not defined for operators"
    fromInteger = error "fromInteger is not defined for operators"

-- | The adjoint operation on complex numbers, kets,
--   bras, and operators.
class Dagger a b | a -> b where
    dagger :: a -> b

instance Dagger Ket Bra where
    dagger (Ket v) = Bra (conjV v)

instance Dagger Bra Ket where
    dagger (Bra v) = Ket (conjV v)

instance Dagger Operator Operator where
    dagger (Operator m) = Operator (conjugateTranspose m)

instance Dagger C C where
    dagger c = conjugate c

class HasNorm a where
    norm      :: a -> Double
    normalize :: a -> a

instance HasNorm Ket where
    norm (Ket v) = M.norm v
    normalize k  = (1 / norm k :+ 0) <> k

instance HasNorm Bra where
    norm (Bra v) = M.norm v
    normalize b  = (1 / norm b :+ 0) <> b

-- | An orthonormal basis of kets.
newtype OrthonormalBasis = OB [Ket]
    deriving (Show)

-- | Make an orthonormal basis from a list of linearly independent kets.
makeOB :: [Ket] -> OrthonormalBasis
makeOB = OB . gramSchmidt

size :: OrthonormalBasis -> Int
size (OB ks) = length ks

listBasis :: OrthonormalBasis -> [Ket]
listBasis (OB ks) = ks

{-
newOrthonormalBasis :: Int -> OrthonormalBasis
newOrthonormalBasis = undefined
-}

class Representable a b | a -> b where
    rep :: OrthonormalBasis -> a -> b
    dim :: a -> Int

instance Representable Ket (Vector C) where
    rep (OB ks) k = fromList (map (\bk -> dagger bk <> k) ks)
    dim (Ket v) = M.dim v

instance Representable Bra (Vector C) where
    rep (OB ks) b = fromList (map (\bk -> b <> bk) ks)
    dim (Bra v) = M.dim v

instance Representable Operator (Matrix C) where
    rep (OB ks) op = fromLists [[ dagger k1 <> op <> k2 | k2 <- ks ] | k1 <- ks ]
    dim (Operator m) = let (p,q) = M.size m
                       in if p == q then p else error "dim: non-square operator"

--------------
-- Spin 1/2 --
--------------

-- | State of a spin-1/2 particle if measurement
--   in the x-direction would give angular momentum +hbar/2.
xp :: Ket
xp = Ket M.xp

-- | State of a spin-1/2 particle if measurement
--   in the x-direction would give angular momentum -hbar/2.
xm :: Ket
xm = Ket M.xm

-- | State of a spin-1/2 particle if measurement
--   in the y-direction would give angular momentum +hbar/2.
yp :: Ket
yp = Ket M.yp

-- | State of a spin-1/2 particle if measurement
--   in the y-direction would give angular momentum -hbar/2.
ym :: Ket
ym = Ket M.ym

-- | State of a spin-1/2 particle if measurement
--   in the z-direction would give angular momentum +hbar/2.
zp :: Ket
zp = Ket M.zp

-- | State of a spin-1/2 particle if measurement
--   in the z-direction would give angular momentum -hbar/2.
zm :: Ket
zm = Ket M.zm

-- | State of a spin-1/2 particle if measurement
--   in the n-direction, described by spherical polar angle theta
--   and azimuthal angle phi, would give angular momentum +hbar/2.
np :: Double -> Double -> Ket
np theta phi
    = (cos (theta / 2) :+ 0) <> zp
      + (sin (theta / 2) :+ 0) * (cos phi :+ sin phi) <> zm

-- | State of a spin-1/2 particle if measurement
--   in the n-direction, described by spherical polar angle theta
--   and azimuthal angle phi, would give angular momentum -hbar/2.
nm :: Double -> Double -> Ket
nm theta phi
    = (sin (theta / 2) :+ 0) <> zp
      - (cos (theta / 2) :+ 0) * (cos phi :+ sin phi) <> zm

-- | The orthonormal basis composed of 'xp' and 'xm'.
xBasis :: OrthonormalBasis
xBasis = makeOB [xp,xm]

-- | The orthonormal basis composed of 'yp' and 'ym'.
yBasis :: OrthonormalBasis
yBasis = makeOB [yp,ym]

-- | The orthonormal basis composed of 'zp' and 'zm'.
zBasis :: OrthonormalBasis
zBasis = makeOB [zp,zm]

-- | Given spherical polar angle theta and azimuthal angle phi,
--   the orthonormal basis composed of 'np' theta phi and 'nm' theta phi.
nBasis :: Double -> Double -> OrthonormalBasis
nBasis theta phi = makeOB [np theta phi,nm theta phi]

-- | The Pauli X operator.
sx :: Operator
sx = xp <> dagger xp - xm <> dagger xm

-- | The Pauli Y operator.
sy :: Operator
sy = yp <> dagger yp - ym <> dagger ym

-- | The Pauli Z operator.
sz :: Operator
sz = zp <> dagger zp - zm <> dagger zm

-- | Pauli operator for an arbitrary direction given
--   by spherical coordinates theta and phi.
sn :: Double -> Double -> Operator
sn theta phi
    = (sin theta * cos phi :+ 0) <> sx +
      (sin theta * sin phi :+ 0) <> sy +
      (cos theta           :+ 0) <> sz

-- | Alternative definition
--   of Pauli operator for an arbitrary direction.
sn' :: Double -> Double -> Operator
sn' theta phi
    = np theta phi <> dagger (np theta phi) -
      nm theta phi <> dagger (nm theta phi)

----------------------
-- Quantum Dynamics --
----------------------

-- | Given a time step and a Hamiltonian operator,
--   produce a unitary time evolution operator.
--   Unless you really need the time evolution operator,
--   it is better to use 'timeEv', which gives the
--   same numerical results without doing an explicit
--   matrix inversion.  The function assumes hbar = 1.
timeEvOp :: Double -> Operator -> Operator
timeEvOp dt (Operator m) = Operator (M.timeEvMat dt m)

-- | Given a time step and a Hamiltonian operator,
--   advance the state ket using the Schrodinger equation.
--   This method should be faster than using 'timeEvOp'
--   since it solves a linear system rather than calculating
--   an inverse matrix.  The function assumes hbar = 1.
timeEv :: Double -> Operator -> Ket -> Ket
timeEv dt (Operator m) (Ket k) = Ket $ M.timeEv dt m k

-----------------
-- Composition --
-----------------

class Kron a where
    kron :: a -> a -> a

instance Kron Ket where
    kron (Ket v1) (Ket v2) = Ket (M.kron v1 v2)

instance Kron Bra where
    kron (Bra v1) (Bra v2) = Bra (M.kron v1 v2)

instance Kron Operator where
    kron (Operator m1) (Operator m2) = Operator (M.kron m1 m2)

-----------------
-- Measurement --
-----------------

-- | The possible outcomes of a measurement
--   of an observable.
--   These are the eigenvalues of the operator
--   of the observable.
possibleOutcomes :: Operator -> [Double]
possibleOutcomes (Operator observable) = M.possibleOutcomes observable

-- | Given an obervable, return a list of pairs
--   of possible outcomes and projectors
--   for each outcome.
outcomesProjectors :: Operator -> [(Double,Operator)]
outcomesProjectors (Operator m)
    = [(val,Operator p) | (val,p) <- M.outcomesProjectors m]

-- | Given an observable and a state ket, return a list of pairs
--   of possible outcomes and probabilites
--   for each outcome.
outcomesProbabilities :: Operator -> Ket -> [(Double,Double)]
outcomesProbabilities (Operator m) (Ket v)
    = M.outcomesProbabilities m v

{-
prob :: Ket -> Ket -> Double
prob k1 k2 = magnitude c ** 2
    where
      c = dagger k1 <> k2

probs :: OrthonormalBasis -> Ket -> [Double]
probs (OB ks) k = map (\bk -> let c = dagger bk <> k in magnitude c ** 2) ks
-}

{-
----------------------------------------
-- Angular Momentum of arbitrary size --
----------------------------------------

angularMomentumZMatrix :: Rational -> Matrix Cyclotomic
angularMomentumZMatrix j
    = case twoJPlusOne j of
        Nothing -> error "j must be a nonnegative integer or half-integer"
        Just d  -> matrix d d (\(r,c) -> if r == c then fromRational (j + 1 - fromIntegral r) else 0)

twoJPlusOne :: Rational -> Maybe Int
twoJPlusOne j
    | j >= 0 && (denominator j == 1 || denominator j == 2)  = Just $ fromIntegral $ numerator (2 * j + 1)
    | otherwise                                             = Nothing

angularMomentumPlusMatrix :: Rational -> Matrix Cyclotomic
angularMomentumPlusMatrix j
    = case twoJPlusOne j of
        Nothing -> error "j must be a nonnegative integer or half-integer"
        Just d  -> matrix d d (\(r,c) -> let mr = j + 1 - fromIntegral r
                                             mc = j + 1 - fromIntegral c
                                         in if mr == mc + 1
                                            then sqrtRat (j*(j+1) - mc*mr)
                                            else 0
                              )

angularMomentumMinusMatrix :: Rational -> Matrix Cyclotomic
angularMomentumMinusMatrix j
    = case twoJPlusOne j of
        Nothing -> error "j must be a nonnegative integer or half-integer"
        Just d  -> matrix d d (\(r,c) -> let mr = j + 1 - fromIntegral r
                                             mc = j + 1 - fromIntegral c
                                         in if mr + 1 == mc
                                            then sqrtRat (j*(j+1) - mc*mr)
                                            else 0
                              )

angularMomentumXMatrix :: Rational -> Matrix Cyclotomic
angularMomentumXMatrix j
    = scaleMatrix (1/2) (angularMomentumPlusMatrix j + angularMomentumMinusMatrix j)

angularMomentumYMatrix :: Rational -> Matrix Cyclotomic
angularMomentumYMatrix j
    = scaleMatrix (1/(2*i)) (angularMomentumPlusMatrix j - angularMomentumMinusMatrix j)

jXMatrix :: Rational -> Matrix Cyclotomic
jXMatrix = angularMomentumXMatrix

jYMatrix :: Rational -> Matrix Cyclotomic
jYMatrix = angularMomentumYMatrix

jZMatrix :: Rational -> Matrix Cyclotomic
jZMatrix = angularMomentumZMatrix

matrixCommutator :: Matrix Cyclotomic -> Matrix Cyclotomic -> Matrix Cyclotomic
matrixCommutator m1 m2 = m1 * m2 - m2 * m1

-----------------------
-- Rotation matrices --
-----------------------

r2i :: Rational -> Integer
r2i r
    | denominator r == 1  = numerator r
    | otherwise           = error "r2i:  not an integer"

-- from Sakurai, revised, (3.8.33)
-- beta in degrees
smallDRotElement :: Rational -> Rational -> Rational -> Rational -> Cyclotomic
smallDRotElement j m' m beta
    = sum [parity(k-m+m') * sqrtRat (fact(j+m) * fact(j-m) * fact(j+m') * fact(j-m'))
                   / fromRational (fact(j+m-k) * fact(k) * fact(j-k-m') * fact(k-m+m'))
                         * cosDeg (beta/2) ^ r2i(2*j-2*k+m-m')
                         * sinDeg (beta/2) ^ r2i(2*k-m+m') | k <- [max 0 (m-m') .. min (j+m) (j-m')]]

parity :: Rational -> Cyclotomic
parity = fromIntegral . parityInteger . r2i

-- | (-1)^n, where n might be negative
parityInteger :: Integer -> Integer
parityInteger n
    | odd n      = -1
    | otherwise  =  1

factInteger :: Integer -> Integer
factInteger n
    | n == 0     = 1
    | n >  0     = n * factInteger (n-1)
    | otherwise  = error "factInteger called on negative argument"

fact :: Rational -> Rational
fact = fromIntegral . factInteger . r2i

-- | Rotation matrix elements.
--   From Sakurai, Revised Edition, (3.5.50).
--   The matrix desribes a rotation by gamma about the z axis,
--   followed by a rotation by beta about the y axis,
--   followed by a rotation by alpha about the z axis.
bigDRotElement :: Rational  -- ^ j, a nonnegative integer or half-integer
               -> Rational  -- ^ m', an integer or half-integer index for the row
               -> Rational  -- ^ m, an integer or half-integer index for the column
               -> Rational  -- ^ alpha, in degrees
               -> Rational  -- ^ beta, in degrees
               -> Rational  -- ^ gamma, in degrees
               -> Cyclotomic  -- ^ rotation matrix element
bigDRotElement j m' m alpha beta gamma
    = polarRat 1 (-(m' * alpha + m * gamma) / 360) * smallDRotElement j m' m beta

-- | Rotation matrix for a spin-j particle.
--   The matrix desribes a rotation by gamma about the z axis,
--   followed by a rotation by beta about the y axis,
--   followed by a rotation by alpha about the z axis.
rotationMatrix :: Rational  -- ^ j, a nonnegative integer or half-integer
               -> Rational  -- ^ alpha, in degrees
               -> Rational  -- ^ beta, in degrees
               -> Rational  -- ^ gamma, in degrees
               -> Matrix Cyclotomic  -- ^ rotation matrix
rotationMatrix j alpha beta gamma
    = case twoJPlusOne j of
        Nothing -> error "bigDRotMatrix: j must be a nonnegative integer or half-integer"
        Just d  -> matrix d d (\(r,c) -> let m' = j + 1 - fromIntegral r
                                             m  = j + 1 - fromIntegral c
                                         in bigDRotElement j m' m alpha beta gamma
                              )

----------------------------------
-- Angular Momentum eigenstates --
----------------------------------

jmColumn :: Rational -> Rational -> Matrix Cyclotomic
jmColumn j m
    = case twoJPlusOne j of
        Nothing -> error "bigDRotMatrix: j must be a nonnegative integer or half-integer"
        Just d  -> matrix d 1 (\(r,_) -> let m' = j + 1 - fromIntegral r
                                         in if m == m'
                                            then 1
                                            else 0
                              )
-}

------------------
-- Gram-Schmidt --
------------------

-- | Form an orthonormal list of kets from
--   a list of linearly independent kets.
gramSchmidt :: [Ket] -> [Ket]
gramSchmidt [] = []
gramSchmidt [k] = [normalize k]
gramSchmidt (k:ks) = let nks = gramSchmidt ks
                         nk = normalize (foldl (-) k [w <> dagger w <> k | w <- nks])
                     in nk:nks

-- todo:  Clebsch-Gordon coeffs
