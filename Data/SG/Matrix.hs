-- SG library
-- Copyright (c) 2009, Neil Brown.
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--  * Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--  * The author's name may not be used to endorse or promote products derived
--    from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
-- IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


-- | A module with various simple matrix operations to augment the vector stuff.
--
-- The Num instances implement proper matrix multiplication as you would expect
-- (not element-wise multiplication).
module Data.SG.Matrix (Matrix22', Matrix33', Matrix44', SquareMatrix(..), Matrix(..),
  identityMatrix, multMatrix, multMatrixGen, translate2D, translate3D, rotateXaxis, rotateYaxis, rotateZaxis) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad.State hiding (mapM)
import Data.Foldable (Foldable, foldr, toList, sum)
import qualified Data.List as List
import Data.Traversable (Traversable, traverse, mapM)
import Prelude hiding (mapM, foldr, sum)

import Data.SG.Vector
import Data.SG.Vector.Basic

-- This function will only work for certain types!  Most importantly, it will not
-- work with lists...
fromList :: (Applicative c, Traversable c) => [a] -> c a
fromList = evalState $ mapM (const $ getHead) $ pure (error "Matrix.fromList")
  where
    getHead = do (x:xs) <- get
                 put xs
                 return x

-- | A square matrix.  You will almost certainly want to use 'Matrix22'' and similar
-- instead of this directly.  It does have a variety of useful instances though,
-- especially 'Functor', 'Num' and 'Matrix'.
--
-- Its definition is based on a square matrix being, for example, a pair of pairs
-- or a triple of triples.
newtype SquareMatrix c a = SquareMatrix (c (c a))

instance Functor c => Functor (SquareMatrix c) where
  fmap f (SquareMatrix m) = SquareMatrix $ fmap (fmap f) m

instance Applicative c => Applicative (SquareMatrix c) where
  pure = SquareMatrix . pure . pure
  (SquareMatrix f) <*> (SquareMatrix m) = SquareMatrix $ (fmap (<*>) f) <*> m
  -- f :: c (c (a -> b))
  -- m :: c (c a)
  -- in ??? <*> m, ??? :: c (c a -> c b)
  -- fmap (<*>) f :: c (c a -> c b)

instance (Foldable c, Applicative c, Eq a) => Eq (SquareMatrix c a) where
  (==) a b = foldr (&&) True $ liftA2 (==) a b
--  (==) (SquareMatrix a) (SquareMatrix b) = and $ zipWith (==) (list a) (list b)
--    where list = concatMap toList . toList

instance Foldable c => Foldable (SquareMatrix c) where
  foldr f x (SquareMatrix m) = foldr (flip $ foldr f) x m

instance Traversable c => Traversable (SquareMatrix c) where
  traverse f (SquareMatrix m) = liftA SquareMatrix $ traverse (traverse f) m

instance (Applicative c, Foldable c, Traversable c, Functor c, Show a) => Show (SquareMatrix c a) where
  show = show . matrixComponents

instance (Read a, Num a, Applicative c, Traversable c) => Read (SquareMatrix c a) where
  readsPrec n s = map (first fromMatrixComponents) $ readsPrec n s

-- | A 2x2 matrix.  Primarily useful via its instances, such as 'Functor', 'Num',
-- and 'Matrix'.
type Matrix22' a = SquareMatrix Pair a
-- | A 3x3 matrix.  Primarily useful via its instances, such as 'Functor', 'Num',
-- and 'Matrix'.
type Matrix33' a = SquareMatrix Triple a
-- | A 4x4 matrix.  Primarily useful via its instances, such as 'Functor', 'Num',
-- and 'Matrix'.
type Matrix44' a = SquareMatrix Quad a

-- | The class that all matrices belong to.
class Matrix m where
  -- | Gives back the matrix as a list of rows.
  matrixComponents :: m a -> [[a]]
  -- | Creates a matrix from a list of rows.  Any missing entries are filled
  -- in with the relevant entries from the identity matrix, hence the identity
  -- matrix is equivalent to @fromMatrixComponents []@.
  fromMatrixComponents :: Num a => [[a]] -> m a

  -- | Transposes a matrix
  transpose :: m a -> m a

-- | The identity matrix.
identityMatrix :: (Num a, Matrix m) => m a
identityMatrix = fromMatrixComponents []

instance (Applicative c, Foldable c, Traversable c, Functor c) => Matrix (SquareMatrix c) where
  matrixComponents (SquareMatrix m) = map toList $ toList m
  fromMatrixComponents = SquareMatrix . fmap fromRow . fromList . zip [0..] . addIdentityRows
    where
      addIdentityRows xs = xs ++ identityRows (length xs)
      identityRow n = replicate n 0 ++ [1] ++ repeat 0
      identityRows n = identityRow n : identityRows (n + 1)
      fromRow (n, r) = fromList $ r ++ drop (length r) (identityRow n)

  -- TODO make this all-functors:
  transpose (SquareMatrix m) = SquareMatrix . fromList . map fromList . List.transpose . map toList . toList $ m

instance (Num a, Traversable c, Foldable c, Functor c, Applicative c) => Num (SquareMatrix c a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  -- Multiplication: hmmmm.
  --
  -- We need to turn each element of the left-hand matrix into an operation on
  -- the whole of the right-hand matrix that will yield the right result.  Each
  -- element needs to operate on its own row from the LHS, and its own column from
  -- the RHS.
  -- 
  (*) (SquareMatrix a) (SquareMatrix b)
    = SquareMatrix $ fmap perRow a
    where
--      sumSetOfRows :: c (c a) -> c a
      sumSetOfRows = foldr (liftA2 (+)) (pure 0)
      
--      perRow :: c a -> c a
      perRow lrow = sumSetOfRows $ liftA2 (\x y -> fmap (*x) y) lrow b

  abs = fmap abs
  negate = fmap negate
  signum = fmap signum
  fromInteger = pure . fromInteger

-- | Matrix multiplication.  There is no requirement that the size of
-- the matrix matches the size of the vector:
--
-- * If the vector is too small for the matrix (e.g. multiplying a 4x4 matrix by
-- a 3x3 vector), 1 will be used for the missing vector entries.
--
-- * If the matrix is too small for the vector (e.g. multiplying a 2x2 matrix by
-- a 3x3 vector), the other components of the vector will be left untouched.
--
-- This allows you to do tricks such as multiplying a 4x4 matrix by a 3D vector,
-- and doing translation (a standard 3D graphics trick).
multMatrixGen :: (Coord p, Matrix m, Num a) => m a -> p a -> p a
multMatrixGen m v = fromComponents $ comps ++ drop (length comps) vc
  where
    comps = [sum $ zipWith (*) r vc | r <- matrixComponents m]
    -- All missing components are 1:
    vc = getComponents v ++ repeat 1

-- | Matrix multiplication where the size of the vector matches the dimensions
-- of the matrix.  The complicated type just means that this function will
-- work for any combination of matrix types and vectors where the width of the
-- square matrix is the same as the number of dimensions in the vector.
multMatrix :: (Foldable c, Applicative c, Num a, IsomorphicVectors c p, IsomorphicVectors p c) => SquareMatrix c a -> p a -> p a
multMatrix (SquareMatrix m) v
  = iso $ fmap (sum . liftA2 (*) (iso v)) m

-- | Given an angle in /radians/, produces a matrix that rotates anti-clockwise
-- by that angle around the Z axis.  Note that this can be used to produce a 2x2
-- (in which case it is a rotation around the origin), 3x3 or 4x4 matrix.
rotateZaxis :: (Floating a, Matrix m) => a -> m a
rotateZaxis t = fromMatrixComponents [[cos t, - sin t], [sin t, cos t]]

-- | Given an angle in /radians/, produces a matrix that rotates anti-clockwise
-- by that angle around the X axis.  Note that this can be used to produce a 2x2,
-- 3x3 or 4x4 matrix, but if you produce a 2x2 matrix, odd things will happen!
rotateXaxis :: (Floating a, Matrix m) => a -> m a
rotateXaxis t = fromMatrixComponents [[1,0,0], [0, cos t, - sin t], [0, sin t, cos t]]

-- | Given an angle in /radians/, produces a matrix that rotates anti-clockwise
-- by that angle around the Y axis.  Note that this can be used to produce a 2x2,
-- 3x3 or 4x4 matrix, but if you produce a 2x2 matrix, odd things will happen!
rotateYaxis :: (Floating a, Matrix m) => a -> m a
rotateYaxis t = fromMatrixComponents [[cos t, 0, - sin t], [0,1,0], [sin t, 0, cos t]]

-- | Given a 2D relative vector, produces a matrix that will translate by that
-- much (when you multiply a 2D point with it using multMatrixGen)
translate2D :: (Num a, IsomorphicVectors p Pair) => p a -> Matrix33' a
translate2D v = SquareMatrix $ Triple
  (Triple (1, 0, x)
  ,Triple (0, 1, y)
  ,Triple (0, 0, 1)
  )
  where
    Pair (x, y) = iso v

-- | Given a 3D relative vector, produces a matrix that will translate by that
-- much (when you multiply a 3D point with it using multMatrixGen)
translate3D :: (Num a, IsomorphicVectors p Triple) => p a -> Matrix44' a
translate3D v = SquareMatrix $ Quad
  (Quad (1, 0, 0, x)
  ,Quad (0, 1, 0, y)
  ,Quad (0, 0, 1, z)
  ,Quad (0, 0, 0, 1)
  )
  where
    Triple (x, y, z) = iso v
