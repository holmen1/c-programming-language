{-# LANGUAGE FlexibleContexts #-}

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