{-# OPTIONS_GHC -Wall #-}

-- | Damped harmonic oscillator
module Main where

import Graphics.Gnuplot.Simple
import Physics.Learn.RungeKutta
  ( integrateSystem
  )

dampedOscillator
  :: Double
  -> Double
  -> Double
  -> (Double, Double, Double)
  -> (Double, Double, Double)
dampedOscillator r l c (_t, vc, il) =
  (1, -vc / r / c - il / c, vc / l)

theStates :: [(Double, Double, Double)]
theStates = integrateSystem (dampedOscillator 10000 200 0.001) 0.01 (0, 1, 0)

plot2 :: IO ()
plot2 =
  plotList
    [ Title "Damped Harmonic Oscillator"
    , XLabel "Time (s)"
    , YLabel "Voltage (V)"
    , Key Nothing
    ]
    (map (\(t, x, _) -> (t, x)) $ take 1000 theStates)

main :: IO ()
main = main2

main2 :: IO ()
main2 =
  plotPath
    [ Title "Damped Harmonic Oscillator"
    , XLabel "Time (s)"
    , YLabel "Voltage (V)"
    , Key Nothing
    , PNG "learn-physics-DHO.png"
    ]
    (map (\(t, x, _) -> (t, x)) $ take 1000 theStates)
    >> putStrLn "output sent to file learn-physics-DHO.png"
