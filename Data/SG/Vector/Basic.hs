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


-- | Some types that are very basic vectors.  Most of the use that can be made
-- of the vectors is in their type-class instances, which support a powerful set
-- of operations.  For example:
--
-- > fmap (*3) v -- Scales vector v by 3
-- > pure 0 -- Creates a vector filled with zeroes
-- > v + w -- Adds two vectors (there is a 'Num' instance, basically)
--
-- Plus all the instances for the classes in "Data.SG.Vector", which allows you
-- to use 'getX' and so on.
--
-- You will probably want to create more friendly type synonyms, such as:
--
-- > type Vector2 = Pair Double
-- > type Vector3 = Triple Double
-- > type Line2 = LinePair Double
-- > type Line3 = LineTriple Double
module Data.SG.Vector.Basic where

import Control.Applicative
import Data.Foldable
import Data.Traversable

import Data.SG.Vector

-- | A pair, which acts as a 2D vector.
newtype Pair a = Pair (a, a)
  deriving (Eq, Ord, Show, Read)
-- | A triple, which acts as a 3D vector.
newtype Triple a = Triple (a, a, a)
  deriving (Eq, Ord, Show, Read)
-- | A quad, which acts as a 4D vector.
newtype Quad a = Quad (a, a, a, a)
  deriving (Eq, Ord, Show, Read)

-- | A pair of (position vector, direction vector) to be used as a 2D line.
newtype LinePair a = LinePair (Pair a, Pair a)
  deriving (Eq, Ord, Show, Read)
-- | A pair of (position vector, direction vector) to be used as a 3D line.
newtype LineTriple a = LineTriple (Triple a, Triple a)
  deriving (Eq, Ord, Show, Read)

instance VectorNum Pair where
  fmapNum1 = fmap
  fmapNum1inv = fmap
  fmapNum2 = liftA2
  simpleVec = pure

instance VectorNum Triple where
  fmapNum1 = fmap
  fmapNum1inv = fmap
  fmapNum2 = liftA2
  simpleVec = pure

instance VectorNum Quad where
  fmapNum1 = fmap
  fmapNum1inv = fmap
  fmapNum2 = liftA2
  simpleVec = pure

instance (Show a, Eq a, Num a) => Num (Pair a) where
  (+) = fmapNum2 (+)
  (-) = fmapNum2 (-)
  (*) = fmapNum2 (*)
  abs = fmapNum1inv abs
  signum = fmapNum1 signum
  negate = fmapNum1inv negate
  fromInteger = simpleVec . fromInteger

instance (Show a, Eq a, Num a) => Num (Triple a) where
  (+) = fmapNum2 (+)
  (-) = fmapNum2 (-)
  (*) = fmapNum2 (*)
  abs = fmapNum1inv abs
  signum = fmapNum1 signum
  negate = fmapNum1inv negate
  fromInteger = simpleVec . fromInteger

instance (Show a, Eq a, Num a) => Num (Quad a) where
  (+) = fmapNum2 (+)
  (-) = fmapNum2 (-)
  (*) = fmapNum2 (*)
  abs = fmapNum1inv abs
  signum = fmapNum1 signum
  negate = fmapNum1inv negate
  fromInteger = simpleVec . fromInteger

instance Applicative Pair where
  pure a = Pair (a, a)
  (<*>) (Pair (fa, fb)) (Pair (a, b)) = Pair (fa a, fb b)

instance Foldable Pair where
  foldr f t (Pair (x, y)) = x `f` (y `f` t)

instance Traversable Pair where
  traverse f (Pair (x, y)) = Pair <$> liftA2 (,) (f x) (f y)

instance Applicative Triple where
  pure a = Triple (a, a, a)
  (<*>) (Triple (fa, fb, fc)) (Triple (a, b, c)) = Triple (fa a, fb b, fc c)

instance Foldable Triple where
  foldr f t (Triple (x, y, z)) = x `f` (y `f` (z `f` t))

instance Traversable Triple where
  traverse f (Triple (x, y, z)) = Triple <$> liftA3 (,,) (f x) (f y) (f z)

instance Applicative Quad where
  pure a = Quad (a, a, a, a)
  (<*>) (Quad (fa, fb, fc, fd)) (Quad (a, b, c, d))
    = Quad (fa a, fb b, fc c, fd d)

instance Foldable Quad where
  foldr f t (Quad (x, y, z, a)) = x `f` (y `f` (z `f` (a `f` t)))

instance Traversable Quad where
  traverse f (Quad (x, y, z, a)) = Quad <$> ((,,,) <$> f x <*> f y <*> f z <*> f a)


instance Functor Pair where
  fmap = fmapDefault

instance Functor Triple where
  fmap = fmapDefault

instance Functor Quad where
  fmap = fmapDefault

instance Coord Pair where
  getComponents (Pair (a, b)) = [a, b]
  fromComponents (a:b:_) = Pair (a, b)
  fromComponents xs = fromComponents $ xs ++ repeat 0

instance Coord2 Pair where
  getX (Pair (a, _)) = a
  getY (Pair (_, b)) = b

instance Coord Triple where
  getComponents (Triple (a, b, c)) = [a, b, c]
  fromComponents (a:b:c:_) = Triple (a, b, c)
  fromComponents xs = fromComponents $ xs ++ repeat 0

instance Coord2 Triple where
  getX (Triple (a, _, _)) = a
  getY (Triple (_, b, _)) = b

instance Coord3 Triple where
  getZ (Triple (_, _, c)) = c


instance Coord Quad where
  getComponents (Quad (a, b, c, d)) = [a, b, c, d]
  fromComponents (a:b:c:d:_) = Quad (a, b, c, d)
  fromComponents xs = fromComponents $ xs ++ repeat 0

instance Coord2 Quad where
  getX (Quad (a, _, _, _)) = a
  getY (Quad (_, b, _, _)) = b

instance Coord3 Quad where
  getZ (Quad (_, _, c, _)) = c


