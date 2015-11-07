{-# OPTIONS_GHC -Wall #-}

{- | 
Module      :  Physics.Learn.Visual.PlotTools
Copyright   :  (c) Scott N. Walck 2011-2014
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

This module contains helping functions for using Gnuplot.
-}

module Physics.Learn.Visual.PlotTools
    ( label
    , postscript
    , psFile
    , examplePlot1
    , examplePlot2
    , plotXYCurve
    )
    where

import Graphics.Gnuplot.Simple
    ( Attribute(..)
    , plotFunc
    , plotPath
    )
import Physics.Learn.Curve
    ( Curve(..)
    )
import Physics.Learn.Position
    ( cartesianCoordinates
    )

-- | An 'Attribute' with a given label at a given position.
label :: String -> (Double,Double) -> Attribute
label name (x,y) 
  = Custom "label" [show name ++ " at " ++ show x ++ "," ++ show y]

-- | An 'Attribute' that requests postscript output.
postscript :: Attribute
postscript = Custom "term" ["postscript"]

-- | An 'Attribute' giving the postscript file name.
psFile :: FilePath -> Attribute
psFile file = Custom "output" [show file]

-- | An example of the use of 'label'.  See the source code.
examplePlot1 :: IO ()
examplePlot1 = plotFunc [Title "Cosine Wave"
                        ,XLabel "Time (ms)"
                        ,YLabel "Velocity"
                        ,label "Albert Einstein" (2,0.8)
                        ] [0,0.01..10::Double] cos

-- | An example of the use of 'postscript' and 'psFile'.  See the source code.
examplePlot2 :: IO ()
examplePlot2 = plotFunc [Title "Cosine Wave"
                        ,XLabel "Time (ms)"
                        ,YLabel "Velocity of Car"
                        ,label "Albert Einstein" (2,0.8)
                        ,postscript
                        ,psFile "post1.ps"
                        ] [0,0.01..10::Double] cos

-- | Plot a Curve in the xy plane using Gnuplot
plotXYCurve :: Curve -> IO ()
plotXYCurve (Curve f a b)
    = plotPath [] [(x,y) | t <- [a,a+dt..b]
                  , let (x,y,_) = cartesianCoordinates (f t)]
      where
        dt = (b-a)/1000
