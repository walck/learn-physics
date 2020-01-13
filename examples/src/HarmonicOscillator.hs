{-# OPTIONS_GHC -Wall #-}

module Main where

import Graphics.Gloss
    ( black
    , display
    , Display(..)
    )
import Physics.Learn.Schrodinger1D
    ( stateVectorFromWavefunction
    , gaussian
    , xRange
    , picture
    )

main1 :: IO ()
main1 = display (InWindow "Probability Wave" (1920,1080) (0,0))
        black (picture (0,1) (xRange (-10) 10 501) (stateVectorFromWavefunction (-10) 10 501
                                     (gaussian 1 1)))

main :: IO ()
main = main1

