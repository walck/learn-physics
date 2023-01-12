{-# OPTIONS_GHC -Wall #-}

-- \^ Nuclear Magnetic Resonance on the Bloch Sphere

module Main where

import Physics.Learn.BlochSphere
  ( evolutionBlochSphere
  , hamRabi
  )
import Physics.Learn.QuantumMat
  ( zm
  )

main :: IO ()
main = evolutionBlochSphere zm (hamRabi 10 1 10)
