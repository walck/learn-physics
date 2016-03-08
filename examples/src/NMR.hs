{-# OPTIONS_GHC -Wall #-}

-- ^ Nuclear Magnetic Resonance on the Bloch Sphere

module Main where

import Physics.Learn.QuantumMat
    ( zm
    )
import Physics.Learn.BlochSphere
    ( hamRabi
    , evolutionBlochSphere
    )

main :: IO ()
main = evolutionBlochSphere zm (hamRabi 10 1 10)
