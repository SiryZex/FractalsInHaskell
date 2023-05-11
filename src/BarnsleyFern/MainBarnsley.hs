import BarnsleyFern
import System.IO
import Data.Char

main = do
    putStrLn $ "Choose a number between 1 and 100000:\n"
    iterationsChar <- getLine
    let iterationsInt = check (stringToInt iterationsChar)
    contents <- readFile "random_numbers.txt"
    let randomNumbers = take iterationsInt (map read $ lines contents :: [Int])
    let ps = map toPixel $ points randomNumbers
    let ppm =
            "P3\n" ++
            show width ++ " " ++ show height ++
            "\n255\n" ++
            unlines [if (x, y) `elem` ps then "0 255 0" else "255 255 255" | y <- [0 .. height - 1], x <- [0 .. width - 1]]
    writeFile "fern.ppm" ppm

check :: Int -> Int
check c
  | c < 1 = error "Invalid choice, choose a value higher"
  | c > 100000 = error "Invalid choice, choose a value lower"
  | otherwise = c