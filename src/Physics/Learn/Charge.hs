{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Trustworthy #-}

{- | 
Module      :  Physics.Learn.Charge
Copyright   :  (c) Scott N. Walck 2011-2014
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

This module contains functions for working with charge, electric field,
electric flux, and electric potential.
-}

module Physics.Learn.Charge
    (
    -- * Charge
      Charge
    , ChargeDistribution(..)
    , totalCharge
    -- * Electric Field
    , eField
    , eFieldFromPointCharge
    , eFieldFromLineCharge
    , eFieldFromSurfaceCharge
    , eFieldFromVolumeCharge
    -- * Electric Flux
    , electricFlux
    -- * Electric Potential
    , electricPotentialFromField
    , electricPotentialFromCharge
    )
    where

import Physics.Learn.CarrotVec
    ( magnitude
    , (*^)
    , (^/)
    )
import Physics.Learn.Position
    ( Position
    , ScalarField
    , VectorField
    , displacement
    , addFields
    )
import Physics.Learn.Curve
    ( Curve(..)
    , straightLine
    , simpleLineIntegral
    , dottedLineIntegral
    )
import Physics.Learn.Surface
    ( Surface(..)
    , surfaceIntegral
    , dottedSurfaceIntegral
    )
import Physics.Learn.Volume
    ( Volume(..)
    , volumeIntegral
    )

-- | Electric charge, in units of Coulombs (C)
type Charge = Double

-- | A charge distribution is a point charge, a line charge, a surface charge,
--   a volume charge, or a combination of these.
--   The 'ScalarField' describes a linear charge density, a surface charge density,
--   or a volume charge density.
data ChargeDistribution = PointCharge Charge Position        -- ^ point charge
                        | LineCharge ScalarField Curve       -- ^ 'ScalarField' is linear charge density (C/m)
                        | SurfaceCharge ScalarField Surface  -- ^ 'ScalarField' is surface charge density (C/m^2)
                        | VolumeCharge ScalarField Volume    -- ^ 'ScalarField' is volume charge density (C/m^3)
                        | MultipleCharges [ChargeDistribution]  -- ^ combination of charge distributions

-- | Total charge (in C) of a charge distribution.
totalCharge :: ChargeDistribution -> Charge
totalCharge (PointCharge q _)       = q
totalCharge (LineCharge lambda c)   = simpleLineIntegral 1000 lambda c
totalCharge (SurfaceCharge sigma s) = surfaceIntegral 200 200 sigma s
totalCharge (VolumeCharge rho v)    = volumeIntegral 50 50 50 rho v
totalCharge (MultipleCharges ds)           = sum [totalCharge d | d <- ds]

{-
shiftChargeDistribution :: Displacement -> ChargeDistribution -> ChargeDistribution
shiftChargeDistribution d (Point
-}

-- | Electric field produced by a point charge.
--   The function 'eField' calls this function
--   to evaluate the electric field produced by a point charge.
eFieldFromPointCharge
    :: Charge          -- ^ charge (in Coulombs)
    -> Position        -- ^ of point charge
    -> VectorField     -- ^ electric field (in V/m)
eFieldFromPointCharge q r' r
    = (k * q) *^ d ^/ magnitude d ** 3
      where
        k = 9e9  -- 1 / (4 * pi * epsilon0)
        d = displacement r' r

-- | Electric field produced by a line charge.
--   The function 'eField' calls this function
--   to evaluate the electric field produced by a line charge.
eFieldFromLineCharge
    :: ScalarField     -- ^ linear charge density lambda
    -> Curve           -- ^ geometry of the line charge
    -> VectorField     -- ^ electric field (in V/m)
eFieldFromLineCharge lambda c r
    = k *^ simpleLineIntegral 1000 integrand c
      where
        k = 9e9  -- 1 / (4 * pi * epsilon0)
        integrand r' = lambda r' *^ d ^/ magnitude d ** 3
            where
              d = displacement r' r

-- | Electric field produced by a surface charge.
--   The function 'eField' calls this function
--   to evaluate the electric field produced by a surface charge.
eFieldFromSurfaceCharge
    :: ScalarField     -- ^ surface charge density sigma
    -> Surface         -- ^ geometry of the surface charge
    -> VectorField     -- ^ electric field (in V/m)
eFieldFromSurfaceCharge sigma s r
    = k *^ surfaceIntegral 200 200 integrand s
      where
        k = 9e9  -- 1 / (4 * pi * epsilon0)
        integrand r' = sigma r' *^ d ^/ magnitude d ** 3
            where
              d = displacement r' r

-- | Electric field produced by a volume charge.
--   The function 'eField' calls this function
--   to evaluate the electric field produced by a volume charge.
eFieldFromVolumeCharge
    :: ScalarField     -- ^ volume charge density rho
    -> Volume          -- ^ geometry of the volume charge
    -> VectorField     -- ^ electric field (in V/m)
eFieldFromVolumeCharge rho v r
    = k *^ volumeIntegral 50 50 50 integrand v
      where
        k = 9e9  -- 1 / (4 * pi * epsilon0)
        integrand r' = rho r' *^ d ^/ magnitude d ** 3
            where
              d = displacement r' r

-- | The electric field produced by a charge distribution.
--   This is the simplest way to find the electric field, because it
--   works for any charge distribution (point, line, surface, volume, or combination).
eField :: ChargeDistribution -> VectorField
eField (PointCharge q r') = eFieldFromPointCharge q r'
eField (LineCharge lam c) = eFieldFromLineCharge lam c
eField (SurfaceCharge sig s) = eFieldFromSurfaceCharge sig s
eField (VolumeCharge rho v) = eFieldFromVolumeCharge rho v
eField (MultipleCharges cds) = addFields $ map eField cds

-------------------
-- Electric Flux --
-------------------

-- | The electric flux through a surface produced by a charge distribution.
electricFlux :: Surface -> ChargeDistribution -> Double
electricFlux surf dist = dottedSurfaceIntegral 200 200 (eField dist) surf

------------------------
-- Electric Potential --
------------------------

-- | Electric potential from electric field, given a position to be the zero
--   of electric potential.
electricPotentialFromField :: Position     -- ^ position where electric potential is zero
                           -> VectorField  -- ^ electric field
                           -> ScalarField  -- ^ electric potential
electricPotentialFromField base ef r = -dottedLineIntegral 1000 ef (straightLine base r)

-- | Electric potential produced by a charge distribution.
--   The position where the electric potential is zero is taken to be infinity.
electricPotentialFromCharge :: ChargeDistribution -> ScalarField
electricPotentialFromCharge (PointCharge q r') = ePotFromPointCharge q r'
electricPotentialFromCharge (LineCharge lam c) = ePotFromLineCharge lam c
electricPotentialFromCharge (SurfaceCharge sig s) = ePotFromSurfaceCharge sig s
electricPotentialFromCharge (VolumeCharge rho v) = ePotFromVolumeCharge rho v
electricPotentialFromCharge (MultipleCharges cds) = addFields $ map electricPotentialFromCharge cds

ePotFromPointCharge
    :: Charge          -- ^ charge (in Coulombs)
    -> Position        -- ^ of point charge
    -> ScalarField     -- ^ electric potential
ePotFromPointCharge q r' r
    = (k * q) / magnitude d
      where
        k = 9e9  -- 1 / (4 * pi * epsilon0)
        d = displacement r' r

ePotFromLineCharge
    :: ScalarField     -- ^ linear charge density lambda
    -> Curve           -- ^ geometry of the line charge
    -> ScalarField     -- ^ electric potential
ePotFromLineCharge lambda c r
    = k *^ simpleLineIntegral 1000 integrand c
      where
        k = 9e9  -- 1 / (4 * pi * epsilon0)
        integrand r' = lambda r' / magnitude d
            where
              d = displacement r' r

ePotFromSurfaceCharge
    :: ScalarField     -- ^ surface charge density sigma
    -> Surface         -- ^ geometry of the surface charge
    -> ScalarField     -- ^ electric potential
ePotFromSurfaceCharge sigma s r
    = k *^ surfaceIntegral 200 200 integrand s
      where
        k = 9e9  -- 1 / (4 * pi * epsilon0)
        integrand r' = sigma r' / magnitude d
            where
              d = displacement r' r

ePotFromVolumeCharge
    :: ScalarField     -- ^ volume charge density rho
    -> Volume          -- ^ geometry of the volume charge
    -> ScalarField     -- ^ electric potential
ePotFromVolumeCharge rho v r
    = k *^ volumeIntegral 50 50 50 integrand v
      where
        k = 9e9  -- 1 / (4 * pi * epsilon0)
        integrand r' = rho r' / magnitude d
            where
              d = displacement r' r

{-
Student Exercise:  Write a function for electric potential difference.

-- | The electric potential difference V(end) - V(beginning) between the endpoints
--   of a curve.
electricPotentialDifference :: Curve -> ChargeDistribution -> Double
electricPotentialDifference c dist = -dottedLineIntegral 1000 (eField dist) c
-}

---------------------------------
-- Common Charge Distributions --
---------------------------------

