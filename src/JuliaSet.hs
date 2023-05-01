module JuliaSet 
( pixelToComplex
, color
, julia
, width
, height
) where

import Data.Complex

width, height :: Int
width = 600
height = 480

maxIter :: Int
maxIter = 1000

escapeRadius :: Double
escapeRadius = 2

julia :: Complex Double -> Complex Double -> Int
julia c z = length $ takeWhile (\z -> magnitude z <= escapeRadius) $ take maxIter $ iterate (\z -> z^2 + c) z

pixelToComplex :: (Int, Int) -> Complex Double
pixelToComplex (x, y) = (fromIntegral x / fromIntegral width - 0.5) * 4 :+ (fromIntegral y / fromIntegral height - 0.5) * 4

color :: Int -> (Int, Int, Int)
color n | n == maxIter = (0, 0, 0)
        | otherwise = let t = fromIntegral n / fromIntegral maxIter in (round $ t * 255, round $ t * 255, round $ t * 255)
