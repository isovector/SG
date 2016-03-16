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


-- | A module with types to use in a 3D system, and various helper functions.
-- Several more functions are available for use in the "Data.SG.Geometry" module.
module Data.SG.Geometry.ThreeDim where

import Control.Applicative
import Data.Foldable (Foldable(foldr))
import Data.Traversable (Traversable(traverse))

import Data.SG.Geometry
import Data.SG.Vector
import Data.SG.Vector.Basic

-- | A point in 3D space.
newtype Point3' a = Point3 (a, a, a)
  deriving (Eq, Ord, Show, Read)

-- | A relative vector (free vector) in 3D space.  The triple is the x, y, z components,
-- and the last item is the /squared magnitude/ of the vector, which is stored
-- with it to speed up various operations.  It is suggested you use 'makeRel3'
-- to create one of these, unless the magnitude is easily apparent, e.g. @Rel3
-- (0, 1, 1) 2@
data Rel3' a = Rel3 (a, a, a) a
  deriving (Eq, Ord, Show, Read)

-- | Constructs a Rel3' vector
makeRel3 :: Num a => (a, a, a) -> Rel3' a
makeRel3 (x, y, z) = Rel3 (x, y, z) (x * x + y * y + z * z)

instance IsomorphicVectors Rel3' Point3' where
  iso (Rel3 p _) = Point3 p
instance IsomorphicVectors Point3' Rel3' where
  iso (Point3 p) = makeRel3 p

instance IsomorphicVectors Rel3' Triple where
  iso (Rel3 p _) = Triple p
instance IsomorphicVectors Triple Rel3' where
  iso (Triple p) = makeRel3 p

instance IsomorphicVectors Point3' Triple where
  iso (Point3 p) = Triple p
instance IsomorphicVectors Triple Point3' where
  iso (Triple p) = Point3 p

instance VectorNum Rel3' where
  fmapNum1 f (Rel3 (x, y, z) _) = makeRel3 (f x, f y, f z)
  fmapNum2 f (Rel3 (x, y, z) _) (Rel3 (x', y', z') _) = makeRel3 (f x x', f y y', f z z')
  fmapNum1inv f (Rel3 (x, y, z) m) = Rel3 (f x, f y, f z) m
  simpleVec a = Rel3 (a, a, a) (3*a*a)

instance VectorNum Point3' where
  fmapNum1 = fmap
  fmapNum1inv = fmap
  fmapNum2 = liftA2
  simpleVec = pure

instance (Show a, Eq a, Num a) => Num (Rel3' a) where
  (+) = fmapNum2 (+)
  (-) = fmapNum2 (-)
  (*) = fmapNum2 (*)
  abs = fmapNum1inv abs
  signum = fmapNum1 signum
  negate = fmapNum1inv negate
  fromInteger = simpleVec . fromInteger

instance Functor Point3' where
  fmap f (Point3 (x, y, z)) = Point3 (f x, f y, f z)

instance Applicative Point3' where
  pure a = Point3 (a, a, a)
  (<*>) (Point3 (fa, fb, fc)) (Point3 (a, b, c)) = Point3 (fa a, fb b, fc c)

instance Foldable Point3' where
  foldr f t (Point3 (x, y, z)) = x `f` (y `f` (z `f` t))

instance Foldable Rel3' where
  foldr f t (Rel3 (x, y, z) _) = x `f` (y `f` (z `f` t))

instance Traversable Point3' where
  traverse f (Point3 (x, y, z)) = liftA3 (curry3 Point3) (f x) (f y) (f z)
    where
      curry3 g a b c = g (a, b, c)

instance Coord2 Point3' where
  getX (Point3 (a,_,_)) = a
  getY (Point3 (_,b,_)) = b

instance Coord3 Point3' where
  getZ (Point3 (_,_,c)) = c

instance Coord2 Rel3' where
  getX (Rel3 (a, _, _) _) = a
  getY (Rel3 (_, b, _) _) = b

instance Coord3 Rel3' where
  getZ (Rel3 (_, _, c) _) = c

instance Coord Point3' where
  getComponents (Point3 (a, b, c)) = [a, b, c]
  fromComponents (a:b:c:_) = Point3 (a, b, c)
  fromComponents xs = fromComponents $ xs ++ repeat 0

instance Coord Rel3' where
  getComponents (Rel3 (a, b, c) _) = [a, b, c]
  fromComponents (a:b:c:_) = makeRel3 (a, b, c)
  fromComponents xs = fromComponents $ xs ++ repeat 0
  magSq (Rel3 _ msq) = msq
  dotProduct (Rel3 (a, b, c) _) (Rel3 (a', b', c') _)
    = a * a' + b * b' + c * c'

instance Geometry Rel3' Point3' Line3' where
  -- a*x*a*x + a*y*a*y = a^2 * (x^2 + y^2)
  scaleRel a (Rel3 (x, y, z) m) = Rel3 (a*x, a*y, a*z) (a*a*m)
  plusDir (Point3 (x, y, z)) (Rel3 (x', y', z') _)
    = Point3 (x + x', y + y', z + z')
  fromPt (Point3 (x, y, z)) (Point3 (x', y', z'))
    = makeRel3 (x - x', y - y', z - z')
  getLineVecs (Line3 pt dir) = (pt, dir)
  makeLine = Line3

------------------------------------------------------------
-- Line stuff:
------------------------------------------------------------

-- | A line in 3D space.  A line is a point and a free vector indicating
--  direction.  A line may be treated by a function as either finite (taking
--  the magnitude of the free vector as the length) or infinite (ignoring the
--  magnitude of the direction vector).
data Line3' a = Line3 {getLineStart3 :: (Point3' a) , getLineDir3 :: (Rel3' a)}
  deriving (Eq, Show, Read)

