{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Safe #-}

{- | 
Module      :  Physics.Learn.SimpleVec
Copyright   :  (c) Scott N. Walck 2012-2018
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

Basic operations on the vector type 'Vec', such as vector addition
and scalar multiplication.
This module is simple in the sense that the operations
on vectors all have simple, concrete types,
without the need for type classes.
This makes using and reasoning about vector operations
easier for a person just learning Haskell.
-}

module Physics.Learn.SimpleVec
    ( Vec
    , R
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
    where

import Physics.Learn.CommonVec
    ( Vec(..)
    , vec
    , iHat
    , jHat
    , kHat
    , (><)
    )

infixl 6 ^+^
infixl 6 ^-^
infixl 7 *^
infixl 7 ^*
infixl 7 ^/
infixl 7 <.>

type R = Double

-- | The zero vector.
zeroV :: Vec
zeroV = vec 0 0 0

-- | The additive inverse of a vector.
negateV :: Vec -> Vec
negateV (Vec ax ay az) = Vec (-ax) (-ay) (-az)

-- | Sum of a list of vectors.
sumV :: [Vec] -> Vec
sumV = foldr (^+^) zeroV

-- | Vector addition.
(^+^) :: Vec -> Vec -> Vec
Vec ax ay az ^+^ Vec bx by bz
    = Vec (ax+bx) (ay+by) (az+bz)

-- | Vector subtraction.
(^-^) :: Vec -> Vec -> Vec
Vec ax ay az ^-^ Vec bx by bz = Vec (ax-bx) (ay-by) (az-bz)

-- | Scalar multiplication, where the scalar is on the left
--   and the vector is on the right.
(*^) :: R -> Vec -> Vec
c *^ Vec ax ay az = Vec (c*ax) (c*ay) (c*az)

-- | Scalar multiplication, where the scalar is on the right
--   and the vector is on the left.
(^*) :: Vec -> R -> Vec
Vec ax ay az ^* c = Vec (c*ax) (c*ay) (c*az)

-- | Division of a vector by a scalar.
(^/) :: Vec -> R -> Vec
Vec ax ay az ^/ c = Vec (ax/c) (ay/c) (az/c)

-- | Dot product of two vectors.
(<.>) :: Vec -> Vec -> R
Vec ax ay az <.> Vec bx by bz = ax*bx + ay*by + az*bz

-- | Magnitude of a vector.
magnitude :: Vec -> R
magnitude v = sqrt(v <.> v)

