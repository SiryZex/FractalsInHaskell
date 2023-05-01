import SierpinskiTriangle

main = do
  let n = 8
      triangle = sierpinski n
      ppm = toPPM $ drawTriangle (2 ^ n) triangle
  writeFile "sierpinski.ppm" ppm

