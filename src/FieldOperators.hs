module FieldOperators
( laplace
, printarr
) where  

import Data.Array.Repa as Repa

dirs = [Z :. 1 :. 0, Z:. (-1) :. 0, Z :. 0 :. 1, Z :. 0 :. (-1)]

inRange = inShapeRange (Z :. 0 :. 0)

--"s" is a scalar field, "v" a vector field
laplace :: Array U DIM2 Float -> Array U DIM2 Float
laplace s = computeS $ Repa.traverse s id laplacecell
    where laplacecell getter index = foldl combine (getter index * (-4)) dirs
            where
                combine :: Float -> DIM2 -> Float
                combine net dir
                    | inRange (extent s) (addDim dir index) = net + getter (addDim dir index)
                    | otherwise                             = net

pad :: String -> Int -> String
pad str len
    | length str < len = pad (str Prelude.++ " ") len
    | otherwise        = str

printarr :: Array U DIM2 Float -> IO ()
printarr arr = putStrLn $ concat [genstring i j | j <- [0..(y-1)], i <- [0..(x-1)]]
    where
        x = head (listOfShape $ extent arr)
        y = last(listOfShape $ extent arr)
        genstring i j
                | i==x-1    = pad (show $ arr ! (Z :. i :. j)) 7 Prelude.++ "\n"
                | otherwise = pad (show $ arr ! (Z :. i :. j)) 7