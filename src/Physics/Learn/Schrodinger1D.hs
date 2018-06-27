{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Trustworthy #-}

{- | 
Module      :  Physics.Learn.Schrodinger1D
Copyright   :  (c) Scott N. Walck 2015-2017
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  experimental

This module contains functions to
solve the (time dependent) Schrodinger equation
in one spatial dimension for a given potential function.
-}

module Physics.Learn.Schrodinger1D
    (
    -- * Potentials
      freeV
    , harmonicV
    , squareWell
    , doubleWell
    , stepV
    , wall
    -- * Initial wavefunctions
--    , harm
    , coherent
    , gaussian
    , movingGaussian
    -- * Utilities
    , stateVectorFromWavefunction
    , hamiltonianMatrix
    , expectX
    , picture
    )
    where

import Data.Complex
    ( Complex(..)
    , magnitude
    )
import Graphics.Gloss
    ( Picture(..)
    , yellow
    )
-- import Math.Polynomial.Hermite
--     ( evalPhysHermite
--     )
import Numeric.LinearAlgebra
    ( R
    , C
    , Vector
    , Matrix
    , (|>)
    , (<.>)
    , fromLists
    , toList
    )
import Physics.Learn.QuantumMat
    ( probVector
    , timeEv
    )

hbar :: Double
hbar = 1

--i :: Complex Double
--i = 0 :+ 1

----------------
-- Potentials --
----------------

-- | Free potential.
--   The potential energy is zero everywhere.
freeV
    :: Double  -- ^ position
    -> Double  -- ^ potential energy
freeV _x = 0

-- | Harmonic potential.
--   This is the potential energy of a linear spring.
harmonicV
    :: Double  -- ^ spring constant
    -> Double  -- ^ position
    -> Double  -- ^ potential energy
harmonicV k x = k * x**2 / 2

-- | A double well potential.
--   Potential energy is a quartic function of position
--   that gives two wells, each approximately harmonic
--   at the bottom of the well.
doubleWell
    :: Double  -- ^ width (for both wells and well separation)
    -> Double  -- ^ energy height of barrier between wells
    -> Double  -- ^ position
    -> Double  -- ^ potential energy
doubleWell a v0 x = v0 * ((x**2 - a**2)/a**2)**2

-- | Finite square well potential.
--   Potential is zero inside the well,
--   and constant outside the well.
--   Well is centered at the origin.
squareWell
    :: Double  -- ^ well width
    -> Double  -- ^ energy height of well
    -> Double  -- ^ position
    -> Double  -- ^ potential energy
squareWell l v0 x
    | abs x < l/2  = 0
    | otherwise    = v0

-- | A step barrier potential.
--   Potential is zero to left of origin.
stepV
    :: Double  -- ^ energy height of barrier (to the right of origin)
    -> Double  -- ^ position
    -> Double  -- ^ potential energy
stepV v0 x
    | x < 0      = 0
    | otherwise  = v0

-- | A potential barrier with thickness and height.
wall
    :: Double  -- ^ thickness of wall
    -> Double  -- ^ energy height of barrier
    -> Double  -- ^ position of center of barrier
    -> Double  -- ^ position
    -> Double  -- ^ potential energy
wall w v0 x0 x
    | abs (x-x0) < w/2  = v0
    | otherwise         = 0

---------------------------
-- Initial wavefunctions --
---------------------------

-- -- | Harmonic oscillator stationary state
-- harm :: Int          -- ^ nonnegative integer n identifying stationary state
--      -> Double       -- ^ x / sqrt(hbar/(m * omega)), i.e. position
--                      --   in units of sqrt(hbar/(m * omega))
--      -> C            -- ^ complex amplitude
-- harm n u
--     = exp (-u**2/2) * evalPhysHermite n u / sqrt (2^n * fact n * sqrt pi) :+ 0

coherent
    :: Double                    -- ^ mass of particle
    -> Double                    -- ^ angular frequency
    -> Complex Double            -- ^ parameter z
    -> Double -> Complex Double  -- ^ wavefunction
coherent m omega z x
    = ((m*omega/(pi*hbar))**0.25 * exp(-m*omega*x**2/(2*hbar)) :+ 0)
      * exp(-z**2/2 + (sqrt(2*m*omega/hbar) * x :+ 0) * z)

gaussian
    :: Double                    -- ^ width parameter
    -> Double                    -- ^ center of wave packet
    -> Double -> Complex Double  -- ^ wavefunction
gaussian a x0 x = exp(-(x-x0)**2/(2*a**2)) / sqrt(a * sqrt pi) :+ 0

movingGaussian
    :: Double                    -- ^ width parameter
    -> Double                    -- ^ center of wave packet
    -> Double                    -- ^ momentum
    -> Double -> Complex Double  -- ^ wavefunction
movingGaussian a x0 p0 x = exp((0 :+ p0*x/hbar) - ((x-x0)**2/(2*a**2) :+ 0)) / (sqrt(a * sqrt pi) :+ 0)

---------------
-- Utilities --
---------------

fact :: Int -> Double
fact 0 = 1
fact n = fromIntegral n * fact (n-1)

linspace :: Double -> Double -> Int -> [Double]
linspace left right num
    = let dx = (right - left) / fromIntegral (num - 1)
      in [ left + dx * fromIntegral n | n <- [0..num-1]]

-- | Transform a wavefunction into a state vector.
stateVectorFromWavefunction :: R         -- ^ lowest x
                            -> R         -- ^ highest x
                            -> Int       -- ^ dimension of state vector
                            -> (R -> C)  -- ^ wavefunction
                            -> Vector C  -- ^ state vector
stateVectorFromWavefunction left right num psi
    = (num |>) [psi x | x <- linspace left right num]

hamiltonianMatrix :: R         -- ^ lowest x
                  -> R         -- ^ highest x
                  -> Int       -- ^ dimension of state vector
                  -> R         -- ^ hbar
                  -> R         -- ^ mass
                  -> (R -> R)  -- ^ potential energy function
                  -> Matrix C  -- ^ Hamiltonian Matrix
hamiltonianMatrix xmin xmax num hbar m pe
    = let coeff = -hbar**2/(2*m)
          dx = (xmax - xmin) / fromIntegral (num - 1)
          diagKEterm = -2 * coeff / dx**2
          offdiagKEterm = coeff / dx**2
          xs = linspace xmin xmax num
      in fromLists [[case abs(i-j) of
                       0  -> (diagKEterm + pe x) :+ 0
                       1  -> offdiagKEterm :+ 0
                       _  -> 0
                          | j <- [1..num] ] | (i,x) <- zip [1..num] xs]

expectX :: Vector C  -- ^ state vector
        -> Vector R  -- ^ vector of x values
        -> R         -- ^ <X>, expectation value of X
expectX psi xs = probVector psi <.> xs


glossScaleX :: Int -> (Double,Double) -> Double -> Float
glossScaleX screenWidth (xmin,xmax) x
    = let w = fromIntegral screenWidth :: Double
      in realToFrac $ (x - xmin) / (xmax - xmin) * w - w / 2

glossScaleY :: Int -> (Double,Double) -> Double -> Float
glossScaleY screenHeight (ymin,ymax) y
    = let h = fromIntegral screenHeight :: Double
      in realToFrac $ (y - ymin) / (ymax - ymin) * h - h / 2

glossScalePoint :: (Int,Int)        -- ^ (screenWidth,screenHeight)
                -> (Double,Double)  -- ^ (xmin,xmax)
                -> (Double,Double)  -- ^ (ymin,ymax)
                -> (Double,Double)  -- ^ (x,y)
                -> (Float,Float)
glossScalePoint (screenWidth,screenHeight) xMinMax yMinMax (x,y)
    = (glossScaleX screenWidth  xMinMax x
      ,glossScaleY screenHeight yMinMax y)


-- | Produce a gloss 'Picture' of state vector
--   for 1D wavefunction.
picture :: (Double, Double)    -- ^ y range
        -> [Double]            -- ^ xs
        -> Vector C            -- ^ state vector
        -> Picture
picture (ymin,ymax) xs psi
    = Color
      yellow
      (Line
       [glossScalePoint
        (screenWidth,screenHeight)
        (head xs, last xs)
        (ymin,ymax)
        p | p <- zip xs (map magSq $ toList psi)])
           where
             magSq = \z -> magnitude z ** 2
             screenWidth  = 1000
             screenHeight =  750

{-
-- | Given an initial state vector and
--   state propagation function, produce a simulation.
--   The 'Float' in the state propagation function is the time
--   interval for one timestep.
simulate1D :: [Double] -> Vector C -> (Float -> (Float,[Double],Vector C) -> (Float,[Double],Vector C)) -> IO ()
simulate1D xs initial statePropFunc
    = simulate display black 10 (0,initial) displayFunc (const statePropFunc)
      where
        display = InWindow "Animation" (screenWidth,screenHeight) (10,10)
        displayFunc (_t,v) = Color yellow (Line [(
      
      white (\tFloat -> Pictures [Color blue (Line (points (realToFrac tFloat)))
                                 ,axes (screenWidth,screenHeight) (xmin,xmax) (ymin,ymax)])

-- | Produce a state propagation function from a time-dependent Hamiltonian.
--   The float is dt.
statePropGloss :: (Double -> Matrix C) -> Float -> (Float,Vector C) -> (Float,Vector C)
statePropGloss ham dt (tOld,v)
    = (tNew, timeEv (realToFrac dt) (ham tMid) v)
      where
        tNew = tOld + dt
        tMid = realToFrac $ (tNew + tOld) / 2

-- | Given an initial state vector and a time-dependent Hamiltonian,
--   produce a visualization of a 1D wavefunction.
evolutionBlochSphere :: Vector C -> (Double -> Matrix C) -> IO ()
evolutionBlochSphere psi0 ham
    = simulateBlochSphere 0.01 psi0 (stateProp ham)

-}


{-
def triDiagMatrixMult(square_arr,arr):
    num = len(arr)
    result = array([0 for n in range(num)],dtype=complex128)
    result[0] = square_arr[0][0] * arr[0] + square_arr[0][1] * arr[1]
    for n in range(1,num-1):
        result[n] = square_arr[n][n-1] * arr[n-1] + square_arr[n][n] * arr[n] \
            + square_arr[n][n+1] * arr[n+1]
    result[num-1] = square_arr[num-1][num-2] * arr[num-2] \
        + square_arr[num-1][num-1] * arr[num-1]
    return result

################
# Main program #
################

if __name__ == '__main__':
    m = 1
    omega = 10
    xmin = -2.0
    xmax =  2.0
#    num = 256
    num = 128
    dt = 0.0002
#    dt = 0.01
    xs = linspace(xmin,xmax,num)
    dx = xs[1] - xs[0]

    super = lambda x: (harm0(m,omega)(x) + harm1(m,omega)(x))/sqrt(2)
    shiftedHarm = lambda x: harm0(m,omega)(x-1)
    coh = coherent(m,omega,1)

#    print sum(conj(psi)*psi)*dx

    harmV = harmonicV(m * omega**2)

#    V = doubleWell(1,0.1*hbar*omega)
    V = squareWell(1.0,hbar*omega)
#    V = harmonicV(m*omega**2)
#    V = stepV(10*hbar*omega)
#    V = wall(0.1,14.0*hbar*omega,0)
#    V = freeV

    H = matrixH(m,xmin,xmax,num,V)
    I = matrixI(num)

    (vals,vecs) = eigh(H)

    E0 = vals[0]
    E1 = vals[1]
    psi0 = normalize(transpose(vecs)[0],dx)
    psi1 = normalize(transpose(vecs)[1],dx)

#    psi = func2psi(gaussian(0.3,1),xmin,xmax,num)
#    psi = func2psi(coh,xmin,xmax,num)
#    psi = func2psi(movingGaussian(0.3,10,-1),xmin,xmax,num)

    psi = psi0
#    psi = psi1
#    psi = (psi0 + psi1)/sqrt(2)

    E = sum(conj(psi)*triDiagMatrixMult(H,psi)).real*dx

    Escale = hbar*omega

    print E
    print Escale

    leftM  = I + 0.5 * i * H / hbar * dt
    rightM = I - 0.5 * i * H / hbar * dt

    box = display(title='Schrodinger Equation',width=1000,height=1000)

    c = curve(pos = psi2rho(psi,xs))
    c.color = color.blue
    c.radius = 0.02

    ball = sphere(radius=0.05,color=color.red,pos=(expectX(psi,xs),0,0))

    pot_curve = [(x,V(x)/Escale,0) for x in xs if V(x)/Escale < xmax]
    pot = curve(color=color.green,pos=pot_curve,radius=0.01)

    Eline = curve(color=(1,1,0),pos=[(x,E/Escale) for x in xs])
#    axis = curve(color=color.white,pos=[(x,0) for x in xs])

    while 1:
        psi = solve(leftM,triDiagMatrixMult(rightM,psi))
        c.pos = psi2rho(psi,xs)
        ball.x = expectX(psi,xs)

To Do:
add combinators for potentials
to shift horizontally and vertically,
and to add potentials

-}

-- Are we committed to SI units for hbar?  No.
-- harmonic oscillator functions depend only on sqrt(hbar/m omega)
-- which is a length parameter
-- for moving gaussian, could give hbar/p0 instead of p0
-- (is that debrogie wavelength?  I think it's h/p0)
