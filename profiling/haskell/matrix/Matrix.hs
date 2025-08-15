
module Matrix where

import Control.DeepSeq (NFData, rnf)
import Data.List (transpose)
instance NFData a => NFData (ListMatrix a) where
    rnf (ListMatrix xs) = rnf xs


-- Define a Matrix typeclass
class Matrix m where
    nrows :: m a -> Int
    ncols :: m a -> Int
    get   :: m a -> Int -> Int -> a
    fromList :: [[a]] -> m a
    toList   :: m a -> [[a]]
    multiply :: Num a => m a -> m a -> m a

newtype ListMatrix a = ListMatrix [[a]] deriving (Show, Eq)

instance Matrix ListMatrix where
    nrows (ListMatrix xs) = length xs
    ncols (ListMatrix xs) = if null xs then 0 else length (head xs)
    get (ListMatrix xs) i j = (xs !! i) !! j
    fromList = ListMatrix
    toList (ListMatrix xs) = xs
    multiply (ListMatrix a) (ListMatrix b) =
        let bt = transpose b
        in ListMatrix [[sum $ zipWith (*) ar bc | bc <- bt] | ar <- a]

