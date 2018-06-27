{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}

{- | 
Module      :  Physics.Learn.BeamStack
Copyright   :  (c) Scott N. Walck 2016-2018
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

Splitters, recombiners, and detectors for Stern-Gerlach
experiments.
-}

-- Spin-1/2 mixed states.

module Physics.Learn.BeamStack
    (
    -- * Core laboratory components
      BeamStack()
    , randomBeam
    , split
    , recombine
    , applyBField
    , dropBeam
    , flipBeams
    , numBeams
    , detect
    -- * Standard splitters
    , splitX
    , splitY
    , splitZ
    -- * Standard magnetic fields
    , applyBFieldX
    , applyBFieldY
    , applyBFieldZ
    -- * Standard combiners
    , recombineX
    , recombineY
    , recombineZ
    -- * Filters
    , xpFilter
    , xmFilter
    , zpFilter
    , zmFilter
    )
    where

import Physics.Learn.QuantumMat
    ( zp
    , zm
    , nm
    , np
    , couter
    , oneQubitMixed
    )
import Numeric.LinearAlgebra
    ( C
    , Vector
    , Matrix
    , iC
    , (<>)
    , kronecker
    , fromLists
    , toList
    , toLists
    , scale
    , size
    , takeDiag
    , ident
    , tr
    )
import Data.Complex
    ( Complex(..)
    , realPart
    , imagPart
    )
import Data.List
    ( intercalate
    )
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

data BeamStack = BeamStack (Matrix C)

showOneBeam :: Double -> String
showOneBeam r = "Beam of intensity " ++ show r

instance Show BeamStack where
    show b = intercalate "\n" $ map showOneBeam (detect b)

{-
unBeamStack :: BeamStack -> Matrix C
unBeamStack (BeamStack m) = m
-}

--------------------
-- Core functions --
--------------------

-- | A beam of randomly oriented spin-1/2 particles.
randomBeam :: BeamStack
randomBeam = BeamStack oneQubitMixed

extendWithZeros :: Matrix C -> Matrix C
extendWithZeros m
    = let (_,q) = size m
          ml = toLists m
      in fromLists $ map (++ [0,0]) ml
             ++ [replicate (q+2) 0, replicate (q+2) 0]

-- reduce row and column size by 2
reduceMat :: Matrix C -> Matrix C
reduceMat m
    = let (p,q) = size m
          ml = toLists m
      in fromLists $ take (p-2) $ map (take (q-2)) ml

checkedRealPart :: C -> Double
checkedRealPart c
    = let eps = 1e-14
      in if imagPart c < eps
         then realPart c
         else error $ "checkRealPart: imagPart = " ++ show (imagPart c)

-- | Return the intensities of a stack of beams.
detect :: BeamStack -> [Double]
detect (BeamStack m)
    = addAlternate $ toList $ takeDiag m

addAlternate :: [C] -> [Double]
addAlternate [] = []
addAlternate [_] = error "addAlternate needs even number of elements"
addAlternate (x:y:xs) = checkedRealPart (x+y) : addAlternate xs

-- | Remove the most recent beam from the stack.
dropBeam :: BeamStack -> BeamStack
dropBeam (BeamStack m) = BeamStack (reduceMat m)

-- | Return the number of beams in a 'BeamStack'.
numBeams :: BeamStack -> Int
numBeams (BeamStack m)
    = let (p,_) = size m
      in p `div` 2

-- | Interchange the two most recent beams on the stack.
flipBeams :: BeamStack -> BeamStack
flipBeams (BeamStack m)
    = let (d,_) = size m
          fl = flipMat d
      in BeamStack $ fl <> m <> tr fl

flipMat :: Int -> Matrix C
flipMat d = bigM d (fromLists [[0,0,1,0]
                              ,[0,0,0,1]
                              ,[1,0,0,0]
                              ,[0,1,0,0]])

-- Turn a 2x2 into a dxd.
bigM2 :: Int -> Matrix C -> Matrix C
bigM2 d m
    | d < 2      = error "bigM2 requires d >= 2"
    | odd d      = error "bigM2 requires even d"
    | otherwise  = fromLists $ map (++ [0,0]) (toLists (ident (d-2)))
                   ++ map (replicate (d-2) 0 ++) (toLists m)

-- Turn a 4x4 into a dxd.
bigM :: Int -> Matrix C -> Matrix C
bigM d m
    | d < 4      = error "bigM requires d >= 4"
    | odd d      = error "bigM requires even d"
    | otherwise  = fromLists $ map (++ [0,0,0,0]) (toLists (ident (d-4)))
                   ++ map (replicate (d-4) 0 ++) (toLists m)

s :: Double -> Double -> Matrix C
s theta phi = kronecker (u `couter` u) (np theta phi `couter` np theta phi)
            + kronecker (l `couter` u) (nm theta phi `couter` nm theta phi)
            + kronecker (u `couter` l) (nm theta phi `couter` nm theta phi)
            + kronecker (l `couter` l) (np theta phi `couter` np theta phi)

u :: Vector C
u = zp

l :: Vector C
l = zm

-- | Given angles describing the orientation of the splitter,
--   removes an incoming beam from the stack and replaces
--   it with two beams, a spin-up and a spin-down beam.
--   The spin-down beam is the most recent beam on the stack.
split :: Double -> Double -> BeamStack -> BeamStack
split theta phi (BeamStack m)
    = let m' = extendWithZeros m
          (p,_) = size m'
          ss = bigM p (s theta phi)
      in BeamStack $ ss <> m' <> tr ss

-- | Given angles describing the orientation of the recombiner,
--   returns a single beam from an incoming pair of beams.
recombine :: Double -> Double -> BeamStack -> BeamStack
recombine theta phi (BeamStack m)
    = let (d,_) = size m
          ss = bigM d (s theta phi)
      in dropBeam $ BeamStack $ ss <> m <> tr ss

mag2x2 :: Double -> Double -> Double -> Matrix C
mag2x2 theta phi omegaT
    = let z = iC * (omegaT :+ 0) / 2
          np' = np theta phi
          nm' = nm theta phi
      in scale (exp   z ) (np' `couter` np')
       + scale (exp (-z)) (nm' `couter` nm')

-- | Given angles describing the direction of a
--   uniform magnetic field, and given an angle
--   describing the product of the Larmor frequency
--   and the time, return an output beam from an
--   input beam.
applyBField :: Double -> Double -> Double -> BeamStack -> BeamStack
applyBField theta phi omegaT (BeamStack m)
    = let (d,_) = size m
          uu = bigM2 d (mag2x2 theta phi omegaT)
      in BeamStack $ uu <> m <> tr uu

-----------------------
-- Derived functions --
-----------------------

-- | A Stern-Gerlach splitter in the x direction.
splitX :: BeamStack -> BeamStack
splitX = split (pi/2) 0

-- | A Stern-Gerlach splitter in the y direction.
splitY :: BeamStack -> BeamStack
splitY = split (pi/2) (pi/2)

-- | A Stern-Gerlach splitter in the z direction.
splitZ :: BeamStack -> BeamStack
splitZ = split 0 0

-- | Given an angle in radians
--   describing the product of the Larmor frequency
--   and the time, apply a magnetic in the x direction
--   to the most recent beam on the stack.
applyBFieldX :: Double -> BeamStack -> BeamStack
applyBFieldX = applyBField (pi/2) 0

-- | Given an angle in radians
--   describing the product of the Larmor frequency
--   and the time, apply a magnetic in the y direction
--   to the most recent beam on the stack.
applyBFieldY :: Double -> BeamStack -> BeamStack
applyBFieldY = applyBField (pi/2) (pi/2)

-- | Given an angle in radians
--   describing the product of the Larmor frequency
--   and the time, apply a magnetic in the z direction
--   to the most recent beam on the stack.
applyBFieldZ :: Double -> BeamStack -> BeamStack
applyBFieldZ = applyBField 0 0

-- | A Stern-Gerlach recombiner in the x direction.
recombineX :: BeamStack -> BeamStack
recombineX = recombine (pi/2) 0

-- | A Stern-Gerlach recombiner in the y direction.
recombineY :: BeamStack -> BeamStack
recombineY = recombine (pi/2) (pi/2)

-- | A Stern-Gerlach recombiner in the z direction.
recombineZ :: BeamStack -> BeamStack
recombineZ = recombine 0 0

-- | Filter for spin-up particles in the x direction.
xpFilter :: BeamStack -> BeamStack
xpFilter = dropBeam . splitX

-- | Filter for spin-down particles in the x direction.
xmFilter :: BeamStack -> BeamStack
xmFilter = dropBeam . flipBeams . splitX

-- | Filter for spin-up particles in the z direction.
zpFilter :: BeamStack -> BeamStack
zpFilter = dropBeam . splitZ

-- | Filter for spin-down particles in the z direction.
zmFilter :: BeamStack -> BeamStack
zmFilter = dropBeam . flipBeams . splitZ
