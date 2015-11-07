{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Safe #-}

{- | 
Module      :  Physics.Learn.CommonVec
Copyright   :  (c) Scott N. Walck 2012-2014
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

This module defines some common vector operations.
It is intended that this module not be imported directly, but that its
functionality be gained by importing either 'SimpleVec' or 'CarrotVec',
but not both.  Choose 'SimpleVec' for vector operations
(such as vector addition) with simple concrete types,
which work only with the type 'Vec' of three-dimensional vectors.
Choose 'CarrotVec' for vector operations that work with any type in the
appropriate type class.
-}

-- The definitions that are common to SimpleVec and CarrotVec.
-- We need to export the data constructor Vec for both SimpleVec and CarrotVec.

module Physics.Learn.CommonVec
    ( Vec(..)
    , vec
    , (><)
    , iHat
    , jHat
    , kHat
    )
    where

infixl 7 ><

-- | A type for vectors.
data Vec = Vec { xComp :: Double  -- ^ x component
               , yComp :: Double  -- ^ y component
               , zComp :: Double  -- ^ z component
               } deriving (Eq)

instance Show Vec where
    show (Vec x y z) = "vec " ++ showDouble x ++ " "
                              ++ showDouble y ++ " "
                              ++ showDouble z

showDouble :: Double -> String
showDouble x
    | x < 0      = "(" ++ show x ++ ")"
    | otherwise  = show x

-- | Form a vector by giving its x, y, and z components.
vec :: Double  -- ^ x component
    -> Double  -- ^ y component
    -> Double  -- ^ z component
    -> Vec
vec = Vec

-- | Cross product.
(><) :: Vec -> Vec -> Vec
Vec ax ay az >< Vec bx by bz = Vec (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)

iHat, jHat, kHat :: Vec
-- | Unit vector in the x direction.
iHat = vec 1 0 0
-- | Unit vector in the y direction.
jHat = vec 0 1 0
-- | Unit vector in the z direction.
kHat = vec 0 0 1
