{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy #-}

{- | 
Module      :  Physics.Learn.CarrotVec
Copyright   :  (c) Scott N. Walck 2011-2014
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

This module defines some basic vector functionality.
It uses the same internal data representation as 'SimpleVec',
but declares 'Vec' to be an instance of 'VectorSpace'.
We import 'zeroV', 'negateV', 'sumV', '^+^', '^-^'
from 'AdditiveGroup', and
'*^', '^*', '^/', '<.>', 'magnitude'
from 'VectorSpace'.

'CarrotVec' exports exactly the same symbols as 'SimpleVec';
they are just defined differently.
-}

-- 2011 Apr 10
-- Definitions common to SimpleVec and CarrotVec have been put in CommonVec.

module Physics.Learn.CarrotVec
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
    where

import Data.VectorSpace
    ( VectorSpace(..)
    , InnerSpace(..)
    , AdditiveGroup(..)
    , Scalar
    , (^+^)
    , (^-^)
    , (*^)
    , (^*)
    , (^/)
    , (<.>)
    , magnitude
    , zeroV
    , negateV
    , sumV
    )
import Physics.Learn.CommonVec
    ( Vec(..)
    , xComp
    , yComp
    , zComp
    , vec
    , (><)
    , iHat
    , jHat
    , kHat
    )

instance AdditiveGroup Vec where
    zeroV = vec 0 0 0
    negateV (Vec ax ay az) = Vec (-ax) (-ay) (-az)
    Vec ax ay az ^+^ Vec bx by bz = Vec (ax+bx) (ay+by) (az+bz)

instance VectorSpace Vec where
    type Scalar Vec = Double
    c *^ Vec ax ay az = Vec (c*ax) (c*ay) (c*az)

instance InnerSpace Vec where
    Vec ax ay az <.> Vec bx by bz = ax*bx + ay*by + az*bz

