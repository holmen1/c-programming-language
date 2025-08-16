module Main where

import MyMatrix

main :: IO ()
main = do
  let m = fromLists [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]] :: Matrix Double
  print m

