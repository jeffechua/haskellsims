module Main
( world0
, waveSim
, main
) where

import FieldOperators

import Data.Array.Repa as Repa
import Graphics.Gloss.Raster.Array as Rarr

disp :: Display
disp = InWindow "Field Test" (250,250) (0,0)

colorize :: World -> Array D DIM2 Color
colorize (World psi _) = Repa.map (\x -> if x > 0 then rgb (x/10) 0 0 else rgb 0 0 (-x/10)) psi

psi0 :: Array U DIM2 Float
psi0 = computeS $ fromFunction (Z :. 50 :. 50) (const 0)
v0 :: Array U DIM2 Float
v0 = computeS $ fromFunction (Z :. 50 :. 50) (\(Z :. x :. y) ->  if (x,y)==(25,25) then 100 else 0)

framerate :: Int
framerate = 30
interval :: Float
interval = 1 / fromIntegral framerate
c2 :: Float
c2 = 10

data World = World (Array U DIM2 Float) (Array U DIM2 Float)
world0 = World psi0 v0

waveSim :: Float -> World -> World
waveSim t (World oldPsi oldV) = World newPsi newV
    where
        newV   = computeS $ oldV   +^ Repa.map (*(interval*c2)) (laplace oldPsi)
        newPsi = computeS $ oldPsi +^ Repa.map (*interval) newV

main :: IO ()
main = playArray disp (5,5) framerate world0 colorize (\event world -> world) waveSim