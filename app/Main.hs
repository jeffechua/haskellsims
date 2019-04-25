module Main
( world0
, waveSim
, main
) where

import FieldOperators

import Data.Array.Repa as Repa
import Graphics.Gloss.Raster.Array as Rarr

width = 400
height = 200
pixSize = 1

disp :: Display
disp = InWindow "Field Test" (width*pixSize,height*pixSize) (0,0)

colorize :: World -> Array D DIM2 Color
colorize (World psi _) = Repa.map (\x -> if x > 0 then rgb (x/10) 0 0 else rgb 0 0 (-x/10)) psi

psi0 :: Array U DIM2 Float
psi0 = computeS $ fromFunction (Z :. height :. width) (const 0)
v0 :: Array U DIM2 Float
v0 = computeS $ fromFunction (Z :. height :. width) (\(Z :. y :. x) ->  if (x-50)*(x-50)+(y-50)*(y-50)<25 then 100 else 0)

framerate :: Int
framerate = 30
interval :: Float
interval = 1 / fromIntegral framerate
c2 :: Float
c2 = 100

data World = World (Array U DIM2 Float) (Array U DIM2 Float)
world0 = World psi0 v0

waveSim :: Float -> World -> World
waveSim t (World oldPsi oldV) = World newPsi newV
    where
        newV   = computeS $ oldV   +^ Repa.map (*(interval*c2)) (laplace oldPsi)
        newPsi = computeS $ oldPsi +^ Repa.map (*interval) newV

main :: IO ()
main = playArray disp (pixSize,pixSize) framerate world0 colorize (\event world -> world) waveSim