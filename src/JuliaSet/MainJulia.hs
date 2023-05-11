import JuliaSet
import Data.Complex

main = do
    putStrLn $ "Choose the number for Julia Fractal you would like to view:\n" ++
        "1) bacteria\n" ++
        "2) 4-leaf clover\n" ++
        "3) hazycloud\n" ++
        "4) dragon\n" ++
        "5) veins\n" ++
        "6) swirlybois\n"++
        "7) cold breath touch\n"
    setFractal <- getLine
    let c = fractal (read setFractal)
    let pixels = [[julia c (pixelToComplex (x, y)) | x <- [0..width-1]] | y <- [0..height-1]]
    let colors = map (map color) pixels
    let ppm = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++
            unlines (map (unwords . map (\(r,g,b) -> show r ++ " " ++ show g ++ " " ++ show b)) colors)
    writeFile "julia.ppm" ppm

fractal :: Int -> Complex Double
fractal c
    | c == 1 = (0.45) :+ (-0.1428) -- bacteria
    | c == 2 = (0.285 :+ 0.01) -- 4-leaf clover
    | c == 3 = (-0.45) :+ (-0.143) -- hazycloud
    | c == 4 = (-0.54) :+ (0.54) -- dragon
    | c == 5 = 0 :+ (0.8) -- veins
    | c == 6 = 0.355 :+ (0.355) -- swirlybois
    | c == 7 = (-0.8) :+ 0.156 -- cold breath touch
    | otherwise = error "Invalid choice"