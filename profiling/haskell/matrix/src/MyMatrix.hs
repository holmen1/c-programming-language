--{-# LANGUAGE FlexibleContexts #-}

module MyMatrix where

import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed (Unbox)

data Matrix a = Matrix
  { nRows :: !Int
  , nCols :: !Int
  , mData :: !(U.Vector a)
  } deriving (Eq, Show)

-- | Create a matrix from a list of rows (each a list of elements)
fromLists :: Unbox a => [[a]] -> Matrix a
fromLists rows =
  let r = length rows
      c = if null rows then 0 else length (head rows)
  in Matrix r c (U.fromList (concat rows))

-- | Add two matrices elementwise
add :: (Unbox a, Num a) => Matrix a -> Matrix a -> Matrix a
add (Matrix r1 c1 v1) (Matrix r2 c2 v2)
  | r1 /= r2 || c1 /= c2 = error "Matrix dimensions must match for addition"
  | otherwise = Matrix r1 c1 (U.zipWith (+) v1 v2)


-- | Matrix multiplication (dot product)
mul :: (Unbox a, Num a) => Matrix a -> Matrix a -> Matrix a
mul (Matrix r1 c1 v1) (Matrix r2 c2 v2)
  | c1 /= r2 = error "Matrix dimensions do not match for multiplication"
  | otherwise = Matrix r1 c2 $ U.generate (r1 * c2) $ \ix ->
      let (i, j) = ix `divMod` c2
      in sum [ (v1 U.! (i * c1 + k)) * (v2 U.! (k * c2 + j)) | k <- [0..c1-1] ]

