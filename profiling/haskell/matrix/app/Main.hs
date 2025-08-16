module Main where

import MyMatrix

main :: IO ()
main = do
  let m1 = fromLists [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]] :: Matrix Double
  let m2 = fromLists [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]] :: Matrix Double
  print m1
  print m2
  print $ add m1 m1
  print $ mul m1 m2

