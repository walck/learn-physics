{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Trustworthy #-}

{- | 
Module      :  Physics.Learn.Current
Copyright   :  (c) Scott N. Walck 2012-2014
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

This module contains functions for working with current, magnetic field,
and magnetic flux.
-}

module Physics.Learn.Current
    (
    -- * Current
      Current
    , CurrentDistribution(..)
    -- * Magnetic Field
    , bField
    , bFieldFromLineCurrent
    , bFieldFromSurfaceCurrent
    , bFieldFromVolumeCurrent
    -- * Magnetic Flux
    , magneticFlux
    )
    where

import Physics.Learn.CarrotVec
    ( magnitude
    , (*^)
    , (^/)
    , (><)
    )
import Physics.Learn.Position
    ( VectorField
    , displacement
    , addFields
    )
import Physics.Learn.Curve
    ( Curve(..)
    , crossedLineIntegral
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

-- | Electric current, in units of Amperes (A)
type Current = Double

-- | A current distribution is a line current (current through a wire), a surface current,
--   a volume current, or a combination of these.
--   The 'VectorField' describes a surface current density
--   or a volume current density.
data CurrentDistribution = LineCurrent Current Curve               -- ^ current through a wire
                         | SurfaceCurrent VectorField Surface      -- ^ 'VectorField' is surface current density (A/m)
                         | VolumeCurrent VectorField Volume        -- ^ 'VectorField' is volume current density (A/m^2)
                         | MultipleCurrents [CurrentDistribution]  -- ^ combination of current distributions

-- | Magnetic field produced by a line current (current through a wire).
--   The function 'bField' calls this function
--   to evaluate the magnetic field produced by a line current.
bFieldFromLineCurrent
    :: Current      -- ^ current (in Amps)
    -> Curve        -- ^ geometry of the line current
    -> VectorField  -- ^ magnetic field (in Tesla)
bFieldFromLineCurrent i c r
    = k *^ crossedLineIntegral 1000 integrand c
      where
        k = 1e-7  -- mu0 / (4 * pi)
        integrand r' = (-i) *^ d ^/ magnitude d ** 3
            where
              d = displacement r' r

-- | Magnetic field produced by a surface current.
--   The function 'bField' calls this function
--   to evaluate the magnetic field produced by a surface current.
--   This function assumes that surface current density
--   will be specified parallel to the surface, and does
--   not check if that is true.
bFieldFromSurfaceCurrent
    :: VectorField  -- ^ surface current density
    -> Surface      -- ^ geometry of the surface current
    -> VectorField  -- ^ magnetic field (in T)
bFieldFromSurfaceCurrent kCurrent c r
    = k *^ surfaceIntegral 100 100 integrand c
      where
        k = 1e-7  -- mu0 / (4 * pi)
        integrand r' = (kCurrent r' >< d) ^/ magnitude d ** 3
            where
              d = displacement r' r

-- | Magnetic field produced by a volume current.
--   The function 'bField' calls this function
--   to evaluate the magnetic field produced by a volume current.
bFieldFromVolumeCurrent
    :: VectorField  -- ^ volume current density
    -> Volume       -- ^ geometry of the volume current
    -> VectorField  -- ^ magnetic field (in T)
bFieldFromVolumeCurrent j c r
    = k *^ volumeIntegral 50 50 50 integrand c
      where
        k = 1e-7  -- mu0 / (4 * pi)
        integrand r' = (j r' >< d) ^/ magnitude d ** 3
            where
              d = displacement r' r

-- | The magnetic field produced by a current distribution.
--   This is the simplest way to find the magnetic field, because it
--   works for any current distribution (line, surface, volume, or combination).
bField :: CurrentDistribution -> VectorField
bField (LineCurrent i c) = bFieldFromLineCurrent i c
bField (SurfaceCurrent kC s) = bFieldFromSurfaceCurrent kC s
bField (VolumeCurrent j v) = bFieldFromVolumeCurrent j v
bField (MultipleCurrents cds) = addFields $ map bField cds

-------------------
-- Magnetic Flux --
-------------------

-- | The magnetic flux through a surface produced by a current distribution.
magneticFlux :: Surface -> CurrentDistribution -> Double
magneticFlux surf dist = dottedSurfaceIntegral 100 100 (bField dist) surf

