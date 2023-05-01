import JuliaSet
import Data.Complex
main = do
    -- let c = (0.45) :+ (-0.1428)
    -- let c = (0.285 :+ 0.01)
    -- let c = (-0.45) :+ (-0.143) -- hazycloud
    -- let c = (-0.54) :+ (0.54) -- dragon
    -- let c = 0 :+ (0.8) -- veins
    let c = 0.355 :+ (0.355) -- swirlybois
    -- let c = (-0.8) :+ 0.156
        pixels = [[julia c (pixelToComplex (x, y)) | x <- [0..width-1]] | y <- [0..height-1]]
        colors = map (map color) pixels
        ppm = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++
              unlines (map (unwords . map (\(r,g,b) -> show r ++ " " ++ show g ++ " " ++ show b)) colors)
    writeFile "julia.ppm" ppm