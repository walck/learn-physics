{-# OPTIONS_GHC -Wall #-}

-- | Damped harmonic oscillator

module Main where

import Physics.Learn.RungeKutta
    ( integrateSystem
    )
import Graphics.Gnuplot.Simple

dampedOscillator :: Double -> Double -> Double
                 -> (Double,Double,Double) -> (Double,Double,Double)
dampedOscillator r l c (_t,vc,il)
    = (1,-vc / r / c - il / c, vc / l)

theStates :: [(Double,Double,Double)]
theStates = integrateSystem (dampedOscillator 10000 200 0.001) 0.01 (0,1,0)

plot2 :: IO ()
plot2 = plotList [Title "Damped Harmonic Oscillator"
                 ,XLabel "Time (s)"
                 ,YLabel "Voltage (V)"
                 ,Key Nothing
                 ] (map (\(t,x,_) -> (t,x)) $ take 1000 theStates)

main :: IO ()
main = plot2
