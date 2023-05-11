module BarnsleyFern
  ( points
  , toPixel
  , height
  , width
  , stringToInt
  )where

width, height :: Int
width = 800
height = 800

nextPoint :: (Float, Float) -> Int -> (Float, Float)
nextPoint (x, y) r
    | r < 1 = (0.0, 0.16 * y)
    | r < 86 = (0.85 * x + 0.04 * y, -0.04 * x + 0.85 * y + 1.6)
    | r < 93 = (0.2 * x - 0.26 * y, 0.23 * x + 0.22 * y + 1.6)
    | otherwise = (-0.15 * x + 0.28 * y, 0.26 * x + 0.24 * y + 0.44)

points :: [Int] -> [(Float, Float)]
points rs = tail $ scanl nextPoint (0, 0) rs

toPixel :: (Float, Float) -> (Int, Int)
toPixel (x, y) =
  ( round $ (x + 3) / 6 * fromIntegral width,
    round $ (11 - y) / 11 * fromIntegral height
  )

stringToInt :: String -> Int
stringToInt s = read s