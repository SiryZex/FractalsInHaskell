module SierpinskiTriangle 
( sierpinski
, drawTriangle
, toPPM
) where

import Data.List (intercalate)

type Point = (Int, Int)

type Triangle = [Point]

sierpinski :: Int -> Triangle
sierpinski 0 = [(0, 0)]
sierpinski n = concatMap (\(x, y) -> [(x, y), (x + m, y), (x, y + m)]) t
  where
    t = sierpinski (n - 1)
    m = 2 ^ (n - 1)

drawTriangle :: Int -> Triangle -> [[(Int, Int, Int)]]
drawTriangle n t = [[if (x, y) `elem` t then (255, 0, 0) else (255, 255, 255) | x <- [0 .. n - 1]] | y <- [0 .. n - 1]]

toPPM :: [[(Int, Int, Int)]] -> String
toPPM s =
  let height = length s
      width = length (head s)
   in intercalate "\n" $
        ["P3", show width ++ " " ++ show height, "255"]
          ++ map unwords (map (map (\(r, g, b) -> unwords [show r, show g, show b])) s)
