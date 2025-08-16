
module Main where

import MyMatrix
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Vector.Unboxed as U

-- Generate a matrix with m[i][j] = i + j
genA :: Int -> Int -> Matrix Double
genA rows cols = Matrix rows cols $ U.generate (rows * cols) $ \ix ->
  let (i, j) = ix `divMod` cols in fromIntegral i + fromIntegral j

-- Generate a matrix with m[i][j] = i * j
genB :: Int -> Int -> Matrix Double
genB rows cols = Matrix rows cols $ U.generate (rows * cols) $ \ix ->
  let (i, j) = ix `divMod` cols in fromIntegral i * fromIntegral j

main :: IO ()
main = do
  let aRows = 300
      aCols = 1000
      bRows = 1000
      bCols = 400

  putStrLn "Generating matrices..."
  let a = genA aRows aCols
      b = genB bRows bCols

  putStrLn "Multiplying matrices..."
  start <- getCurrentTime
  let c = mul a b
  end <- c `seq` getCurrentTime

  putStrLn $ "Matrix multiplication took " ++ show (realToFrac (diffUTCTime end start) :: Double) ++ " seconds"

