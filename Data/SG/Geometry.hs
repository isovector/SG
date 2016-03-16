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


-- | This module has the type-class (and associated functions) for dealing with
-- geometric systems of 2 or 3 dimensions.
module Data.SG.Geometry where

import Control.Arrow
import Data.SG.Vector
import Data.SG.Vector.Basic

-- | A geometry system, parameterised over points, relative (free) vectors, and
-- lines.  There are separate instances for two dimensions and for three dimensions.
-- Each pair of type-class parameters is uniquely determined by the other parameter
-- (i.e. by the dimensionality, and which vector type you are using).
-- 
-- Minimal implementation: everything but scaleRel.
class (VectorNum rel, Coord rel, Coord pt, IsomorphicVectors rel pt, IsomorphicVectors
  pt rel) => Geometry rel pt ln | rel -> pt ln, pt -> rel ln, ln -> rel pt where
  -- | Scales a relative (free) vector by the given amount.
  scaleRel :: Num a => a -> rel a -> rel a
  scaleRel a = fmapNum1 (*a)
  -- | Adds a relative (free) vector to a given point.
  plusDir :: Num a => pt a -> rel a -> pt a
  -- | Determines the relative (free) vector /to/ the first parameter /from/ the
  -- second parameter.  So:
  --
  -- > Point2 (1,8) `fromPt` Point2 (3,4) == Point2 (-2,3)
  fromPt :: Num a => pt a -> pt a -> rel a
  -- | Given a line, converts it back into its point and relative vector.  It should
  -- always be the case that @uncurry makeLine . getLineVecs@ is the identity function.
  getLineVecs :: Num a => ln a -> (pt a, rel a)
  -- | Given a point and relative vector, creates a line.  It should always be
  -- the case that @uncurry makeLine . getLineVecs@ is the identity function.
  makeLine :: Num a => pt a -> rel a -> ln a

instance Geometry Pair Pair LinePair where
  plusDir = (+)
  fromPt = (-)
  getLineVecs (LinePair lp) = lp
  makeLine = curry LinePair

instance Geometry Triple Triple LineTriple where
  plusDir = (+)
  fromPt = (-)
  getLineVecs (LineTriple lp) = lp
  makeLine = curry LineTriple


-- | Adds the negation of the relative (free) vector to the point.
minusDir :: (Num a, Geometry rel pt ln) => pt a -> rel a -> pt a
minusDir p r = p `plusDir` fmapNum1 negate r

-- | The flipped version of 'fromPt'.
toPt :: (Geometry rel pt ln, Num a) => pt a -> pt a -> rel a
toPt = flip fromPt

-- | Gets the line /from/ the first point, /to/ the second point.
lineTo :: (Num a, Geometry rel pt ln) => pt a -> pt a -> ln a
lineTo a b = makeLine a (b `fromPt` a)

-- | The flipped version of 'lineTo'.
lineFrom :: (Num a, Geometry rel pt ln) => pt a -> pt a -> ln a
lineFrom = flip lineTo

-- | Gets the point at the start of the line.
getLineStart :: (Num a, Geometry rel pt ln) => ln a -> pt a
getLineStart = fst . getLineVecs

-- | Gets the direction vector of the line.
getLineDir :: (Num a, Geometry rel pt ln) => ln a -> rel a
getLineDir = snd . getLineVecs

-- | Gets the point at the end of the line.
getLineEnd :: (Geometry rel pt ln, Num a) => ln a -> pt a
getLineEnd = uncurry plusDir . getLineVecs

-- | Alters the line to the given length, but with the same start point and direction.
makeLength :: (Floating a, Ord a, Geometry rel pt ln) => a -> ln a -> ln a
makeLength x = uncurry makeLine . second (scaleRel x . unitVector) . getLineVecs

-- | Given a multiple of the /direction vector/ (this is /not/ distance unless
-- the direction vector is a unit vector), calculates that point.
alongLine :: (Num a, Geometry rel pt ln) => a -> ln a -> pt a
alongLine a = uncurry plusDir . second (scaleRel a) . getLineVecs

-- | Checks if the given point is on the given line (to within a small epsilon-tolerance).
--  If it is, gives back the distance along the line (as a multiple of its direction
-- vector) to the point in a Just wrapper.  If the point is not on the line, Nothing
-- is returned.
distAlongLine :: (Geometry rel pt ln, Ord a, Floating a) => pt a -> ln a -> Maybe a
distAlongLine pt ln
  = if sameDirection lnDir fromStart
      then Just $ mag fromStart
      else Nothing
  where
    fromStart = pt `fromPt` getLineStart ln
    lnDir = getLineDir ln

-- | Checks if the given point is on the given line (to within a small epsilon-tolerance).
isOnLine :: (Geometry rel pt ln, Ord a, Floating a) => pt a -> ln a -> Bool
isOnLine pt ln = sameDirection lnDir fromStart
  where
    fromStart = pt `fromPt` getLineStart ln
    lnDir = getLineDir ln

-- | Finds the nearest point on the line to the given point, and gives back its
-- distance along the line (as a multiple of the direction vector).  Since the
-- nearest distance will be at a right-angle to the point, this is the same as
-- projecting the point onto the line.
nearestDistOnLine :: (Geometry rel pt ln, Ord a, Floating a) =>
  pt a -> ln a -> a
-- The nearest point on the line will be the one forming a right-angle triangle
-- between the line and the point.  We can use the dot product to project the point
-- onto the line.  We want |a| cos theta / |b| for the distance, which is the same
-- as a . b / |b|^2.
nearestDistOnLine pt ln
  | lnDirMagSq == 0 = 0 -- all-zero direction vector
  | otherwise = (fromStart `dotProduct` lnDir) / lnDirMagSq
  where
    fromStart = pt `fromPt` getLineStart ln
    lnDir = getLineDir ln
    lnDirMagSq = magSq lnDir

-- | Finds the nearest point on the line to the given point, and gives back the
-- point.
nearestPointOnLine :: (Geometry rel pt ln, Ord a, Floating a) =>
  pt a -> ln a -> pt a
nearestPointOnLine pt ln = nearestDistOnLine pt ln `alongLine` ln

-- | Gives the distance along the line (2D or 3D) at a given X value.  Returns Nothing
-- if the line is parallel to the YZ plane (in 2D, if the X component of the line
-- is zero).  The value returned is a multiple of the direction vector of the line,
-- which will only be the same as distance if the direction vector is a unit vector.
valueAtX :: (Geometry rel pt ln, Coord2 rel, Coord2 pt, Fractional a)
  => ln a -> a -> Maybe a
valueAtX l tgt
  | xd == 0 = Nothing
  | otherwise = let t = (tgt - x) / xd in Just t
  where
    x = getX $ getLineStart l
    xd = getX $ getLineDir l

-- | Gives the distance along the line (2D or 3D) at a given Y value.  Returns Nothing
-- if the line is parallel to the XZ plane (in 2D, if the Y component of the line
-- is zero).  The value returned is a multiple of the direction vector of the line,
-- which will only be the same as distance if the direction vector is a unit vector.
valueAtY :: (Geometry rel pt ln, Coord2 rel, Coord2 pt, Fractional a)
  => ln a -> a -> Maybe a
valueAtY l tgt
  | yd == 0 = Nothing
  | otherwise = let t = (tgt - y) / yd in Just t
  where
    y = getY $ getLineStart l
    yd = getY $ getLineDir l

-- | Gives the distance along the 3D line at a given Z value.  Returns Nothing
-- if the line is parallel to the XY plane. The value returned is a multiple
-- of the direction vector of the line, which will only be the same as
-- distance if the direction vector is a unit vector.
valueAtZ :: (Geometry rel pt ln, Coord3 rel, Coord3 pt, Fractional a)
  => ln a -> a -> Maybe a
valueAtZ l tgt
  | zd == 0 = Nothing
  | otherwise = let t = (tgt - z) / zd in Just t
  where
    z = getZ $ getLineStart l
    zd = getZ $ getLineDir l

-- | pointAtX (and the Y and Z equivalents) are wrappers around 'valueAtX' (and
-- similar) that give back the point rather than distance along the line.
pointAtX, pointAtY :: (Geometry rel pt ln, Coord2 rel, Coord2 pt, Fractional a)
  => ln a -> a -> Maybe (pt a)
pointAtX l = fmap (flip alongLine l) . valueAtX l
pointAtY l = fmap (flip alongLine l) . valueAtY l

pointAtZ :: (Geometry rel pt ln, Coord3 rel, Coord3 pt, Fractional a)
  => ln a -> a -> Maybe (pt a)
pointAtZ l = fmap (flip alongLine l) . valueAtZ l


