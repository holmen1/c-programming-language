module Main where

import Matrix (ListMatrix(..), fromList, multiply)
import Control.DeepSeq (deepseq)
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- Generate a random matrix of given size with small Double values
randomMatrix :: Int -> Int -> IO (ListMatrix Double)
randomMatrix rows cols = do
    rowsList <- replicateM rows (replicateM cols randomEntry)
    return $ fromList rowsList
  where
    randomEntry = do
      x <- randomRIO (-0.01, 0.01)
      return x

main :: IO ()
main = do
  let aRows = 300
      aCols = 1000
      bRows = 1000
      bCols = 400

  putStrLn "Generating random matrices..."
  a <- randomMatrix aRows aCols
  b <- randomMatrix bRows bCols

  putStrLn "Multiplying matrices..."
  start <- getCurrentTime
  let c = a `multiply` b
  c `deepseq` return ()
  end <- getCurrentTime

  putStrLn $ "Matrix multiplication took " ++ show (realToFrac (diffUTCTime end start) :: Double) ++ " seconds"
-- Matrix multiplication took 2.781441247 seconds
