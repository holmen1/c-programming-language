module Main where

import MyMatrix
import Test.HUnit

main :: IO ()
main = do
  let m1 = fromLists [[1, 2], [3, 4]] :: Matrix Int
      m2 = fromLists [[5, 6], [7, 8]] :: Matrix Int
      m3 = fromLists [[1, 2, 3], [4, 5, 6]] :: Matrix Int
      m4 = fromLists [[7, 8], [9, 10], [11, 12]] :: Matrix Int

      addExpected = fromLists [[6,8],[10,12]] :: Matrix Int
      mulExpected = fromLists [[58,64],[139,154]] :: Matrix Int

      tests = TestList
        [ "add" ~: add m1 m2 ~?= addExpected
        , "mul" ~: mul m3 m4 ~?= mulExpected
        ]
  result <- runTestTT tests
  if errors result + failures result == 0 then putStrLn "All tests passed!" else error "Some tests failed."

