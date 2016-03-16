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


-- | The module with all the different type-classes for vectors.  Generally, the
-- main functions you might need from this function are:
--
-- * 'magSq' and 'mag' (defined for all vectors).
--
-- * 'getX' and 'getY' (defined for all vectors) as well as 'getZ' (defined for
-- all vectors with 3 or more dimensions).
-- 
-- * 'dotProduct', 'unitVector', 'averageVec', 'averageUnitVec', 'sameDirection',
-- 'projectOnto', 'projectPointOnto', 'distFrom' (defined for all vectors).
-- 
-- * 'iso', which is defined for all combinations of vectors with the same number
-- of dimensions.
--
-- The rest of the functions are mainly just wiring necessary for other functions,
-- but must be exported.
--
-- As to the vector types, there are two methods to use this library.  One is to
-- use the types from the "Data.SG.Vector.Basic" library, which support basic vector
-- operations.  The other is to use the types from the "Data.SG.Geometry.TwoDim"
-- and "Data.SG.Geometry.ThreeDim" modules, where a position vector is differentiated
-- from a relative vector (to increase clarity of code, and help prevent errors
-- such as adding two points together).  Both systems can be used with various
-- useful functions (involving lines too) from "Data.SG.Geometry".
module Data.SG.Vector where

import Data.Foldable (Foldable, toList)

-- | An isomorphism amongst vectors.  Allows you to convert between two vectors
-- that have the same dimensions.  You will notice that all the instances reflect
-- this.
class IsomorphicVectors from to where
  iso :: Num a => from a -> to a

instance IsomorphicVectors v v where
  iso = id


-- | The class that is implemented by all vectors.
-- 
-- Minimal implementation: fromComponents
class Foldable p => Coord p where
  -- | Gets the components of the vector, in the order x, y (, z).
  getComponents :: Num a => p a -> [a]
  getComponents = toList
  -- | Re-constructs a vector from the list of coordinates.  If there are too few,
  -- the rest will be filled with zeroes.  If there are too many, the latter ones are
  -- ignored.
  fromComponents :: Num a => [a] -> p a
  -- | Gets the magnitude squared of the vector.  This should be fast for
  -- repeated calls on 'Data.SG.Geometry.TwoDim.Rel2'' and
  -- 'Data.SG.Geometry.ThreeDim.Rel3'', which cache this value.
  magSq :: Num a => p a -> a
  magSq = sum . map (\x -> x * x) . getComponents

  -- | Computes the dot product of the two vectors.
  dotProduct :: Num a => p a -> p a -> a
  dotProduct a b = sum $ zipWith (*) (getComponents a) (getComponents b)

-- | This class is implemented by all 2D and 3D vectors, so 'getX' gets the X co-ordinate
-- of both 2D and 3D vectors.
class Coord p => Coord2 p where
  getX :: p a -> a
  getY :: p a -> a

-- | This class is implemented by all 3D vectors.  To get the X and Y components,
-- use 'getX' and 'getY' from 'Coord2'.
class Coord2 p => Coord3 p where
  getZ :: p a -> a

-- | The origin\/all-zero vector (can be used with any vector type you like)
origin :: (Coord p, Num a) => p a
origin = fromComponents $ repeat 0

-- | Gets the magnitude of the given vector.
mag :: (Coord p, Floating a) => p a -> a
mag = sqrt . magSq

-- | Scales the vector so that it has length 1.  Note that due to floating-point
-- inaccuracies and so on, mag (unitVector v) will not necessarily equal 1, but
-- it should be very close.  If an all-zero vector is passed, the same will be
-- returned.
--
-- This function should be very fast when called on
-- 'Data.SG.Geometry.TwoDim.Rel2'' and 'Data.SG.Geometry.ThreeDim.Rel3'';
-- vectors that are already unit vectors (no processing is done).
unitVector :: (Coord p, VectorNum p, Ord a, Floating a) => p a -> p a
unitVector v
  | abs (magSq v - 1) < 0.000001 = v
  | magSq v == 0 = v -- Avoid division by zero
  | otherwise = fmapNum1 (/ mag v) v

-- | Gets the average vector of all the given vectors.  Essentially it is the
-- sum of the vectors, divided by the length, so @averageVec [Point2 (-3, 0), Point2
-- (5,0)]@ will give @Point2 (1,0)@.  If the list is empty, the
-- all-zero vector is returned.
averageVec :: (Fractional a, VectorNum p, Num (p a)) => [p a] -> p a
averageVec [] = 0
averageVec vs = fmapNum1 (/ fromInteger (toInteger $ length vs)) (sum vs)

-- | Like averageVec composed with unitVector -- gets the average of the
-- vectors in the list, and normalises the length.  If the list is empty, the all-zero
-- vector is returned (which is therefore not a unit vector).  Similarly,
-- if the average of all the vectors is all-zero, the all-zero vector will be returned.
averageUnitVec :: (Floating a, Ord a, Coord p, VectorNum p, Num (p a)) => [p a] -> p a
averageUnitVec [] = 0
averageUnitVec vs = unitVector $ sum vs

-- | Works out if the two vectors are in the same direction (to within a small
-- tolerance).
sameDirection :: (VectorNum rel, Coord rel, Ord a, Floating a) => rel a -> rel a -> Bool
sameDirection v w
  = all (< 0.000001) diffs
  where
    diffs = map abs $ zipWith (-) (getComponents $ unitVector v) (getComponents $ unitVector w)

-- | Gives back the vector (first parameter), translated onto given axis (second
-- parameter).  Note that the scale is always distance, /not/ related to the size
-- of the axis vector.
projectOnto :: (Floating a, Ord a, VectorNum rel, Coord rel) => rel a -> rel a -> a
projectOnto v axis = (v `dotProduct` unitVector axis)

-- | Projects the first parameter onto the given axes (X, Y), returning a point
-- in terms of the new axes.
projectOnto2 :: (Floating a, Ord a, VectorNum rel, Coord rel) =>
  rel a -> (rel a, rel a) -> rel a
projectOnto2 v (axisX, axisY)
  = fromComponents [v `projectOnto` axisX, v `projectOnto` axisY]

-- | Gives back the point (first parameter), translated onto given axis (second
-- parameter).  Note that the scale is always distance, /not/ related to the size
-- of the axis vector.
projectPointOnto :: (Floating a, Ord a, VectorNum rel, Coord rel, IsomorphicVectors pt rel) => pt a -> rel a -> a
projectPointOnto pt = projectOnto (iso pt)

-- | Projects the point (first parameter) onto the given axes (X, Y), returning a point
-- in terms of the new axes.
projectPointOnto2 :: (Floating a, Ord a, VectorNum rel, Coord rel, IsomorphicVectors
  pt rel, Coord pt) => pt a -> (rel a, rel a) -> pt a
projectPointOnto2 v (axisX, axisY)
  = fromComponents [v `projectPointOnto` axisX, v `projectPointOnto` axisY]

-- | Works out the distance between two points.
distFrom :: (VectorNum pt, Coord pt, Floating a) => pt a -> pt a -> a
distFrom v0 v1 = mag $ fmapNum2 (-) v0 v1

-- | A modified version of 'Functor' and 'Control.Applicative.Applicative' that adds the 'Num'
-- constraint on the result.  You are unlikely to need to use this class much
-- directly.  Some vectors have 'Functor' and 'Control.Applicative.Applicative' instances anyway.
class VectorNum f where
  -- | Like 'fmap', but with a 'Num' constraint.
  fmapNum1 :: Num b => (a -> b) -> f a -> f b
  -- | Like 'Control.Applicative.liftA2', but with a 'Num' constraint.
  fmapNum2 :: Num c => (a -> b -> c) -> f a -> f b -> f c
  -- | Like 'fmapNum1', but can only be used if you won't change the magnitude:
  fmapNum1inv :: Num a => (a -> a) -> f a -> f a
  -- | Like 'Control.Applicative.pure' (or 'fromInteger') but with a 'Num' constraint.
  simpleVec :: Num a => a -> f a
