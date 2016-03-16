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


-- | A module with types to use in a 2D system, and various helper functions.
-- Several more functions are available for use in the "Data.SG.Geometry" module.
module Data.SG.Geometry.TwoDim (Point2'(..), Rel2'(..), makeRel2, Line2'(..),
  toAngle, perpendicular2, reflectAgainst2, reflectAgainstIfNeeded2, intersectLines2, findAllIntersections2,
    intersectLineCircle, point2AtZ) where

import Control.Applicative
import Control.Arrow ((&&&))
import Data.Foldable (Foldable(foldr))
import Data.Traversable (Traversable(traverse))
import Data.Maybe

import Data.SG.Vector
import Data.SG.Vector.Basic
import Data.SG.Geometry

-- | A point in 2D space.
newtype Point2' a = Point2 (a, a)
  deriving (Eq, Ord, Show, Read)

-- | A relative vector (free vector) in 2D space.  The pair are the x and y components,
-- and the last item is the /squared magnitude/ of the vector, which is stored
-- with it to speed up various operations.  It is suggested you use 'makeRel2'
-- to create one of these, unless the square magnitude is easily apparent, e.g. @Rel2
-- (0, 2) 4@
data Rel2' a = Rel2 (a,a) a
  deriving (Eq, Ord, Show, Read)

-- | Constructs a Rel2' vector.
makeRel2 :: Num a => (a, a) -> Rel2' a
makeRel2 (x, y) = Rel2 (x, y) (x * x + y * y)

instance IsomorphicVectors Rel2' Point2' where
  iso (Rel2 p _) = Point2 p
instance IsomorphicVectors Point2' Rel2' where
  iso (Point2 p) = makeRel2 p

instance IsomorphicVectors Rel2' Pair where
  iso (Rel2 p _) = Pair p
instance IsomorphicVectors Pair Rel2' where
  iso (Pair p) = makeRel2 p

instance IsomorphicVectors Point2' Pair where
  iso (Point2 p) = Pair p
instance IsomorphicVectors Pair Point2' where
  iso (Pair p) = Point2 p

instance VectorNum Rel2' where
  fmapNum1 f (Rel2 (x, y) _) = makeRel2 (f x, f y)
  fmapNum2 f (Rel2 (x, y) _) (Rel2 (x', y') _) = makeRel2 (f x x', f y y')
  fmapNum1inv f (Rel2 (x, y) m) = Rel2 (f x, f y) m
  simpleVec a = Rel2 (a, a) (2*a*a)

instance VectorNum Point2' where
  fmapNum1 = fmap
  fmapNum1inv = fmap
  fmapNum2 = liftA2
  simpleVec = pure

-- | Multiplication doesn't make much sense, but the rest do!
instance (Show a, Eq a, Num a) => Num (Rel2' a) where
  (+) = fmapNum2 (+)
  (-) = fmapNum2 (-)
  (*) = fmapNum2 (*)
  abs = fmapNum1inv abs
  signum = fmapNum1 signum
  negate = fmapNum1inv negate
  fromInteger = simpleVec . fromInteger

instance Functor Point2' where
  fmap f (Point2 (x, y)) = Point2 (f x, f y)

instance Applicative Point2' where
  pure a = Point2 (a, a)
  (<*>) (Point2 (fa, fb)) (Point2 (a, b)) = Point2 (fa a, fb b)

instance Foldable Point2' where
  foldr f t (Point2 (x, y)) = x `f` (y `f` t)

instance Foldable Rel2' where
  foldr f t (Rel2 (x, y) _) = x `f` (y `f` t)

instance Traversable Point2' where
  traverse f (Point2 (x, y)) = liftA2 (curry Point2) (f x) (f y)

instance Coord2 Point2' where
  getX (Point2 (a, _)) = a
  getY (Point2 (_, b)) = b

instance Coord Point2' where
  getComponents (Point2 (a, b)) = [a, b]
  fromComponents (a:b:_) = Point2 (a, b)
  fromComponents xs = fromComponents $ xs ++ repeat 0

instance Coord2 Rel2' where
  getX (Rel2 (a, _) _) = a
  getY (Rel2 (_, b) _) = b

instance Coord Rel2' where
  getComponents (Rel2 (a, b) _) = [a, b]
  fromComponents (a:b:_) = makeRel2 (a, b)
  fromComponents xs = fromComponents $ xs ++ repeat 0
  magSq (Rel2 _ msq) = msq
  dotProduct (Rel2 (a, b) _) (Rel2 (a', b') _)
    = a * a' + b * b'

instance Geometry Rel2' Point2' Line2' where
  -- a*x*a*x + a*y*a*y = a^2 * (x^2 + y^2)
  scaleRel a (Rel2 (x,y) m) = Rel2 (a*x, a*y) (a*a*m)
  plusDir (Point2 (x, y)) (Rel2 (x', y') _) = Point2 (x + x', y + y')
  fromPt (Point2 (x, y)) (Point2 (x', y')) = makeRel2 (x - x', y - y')
  getLineVecs (Line2 pt dir) = (pt, dir)
  makeLine = Line2

-- | Gets the angle, in /radians/, anti-clockwise from the x-axis.  If you pass
-- the all-zero vector, the return value will be zero.
toAngle :: RealFloat a => Rel2' a -> a
toAngle (Rel2 (x, y) _)
  | x == 0 && y == 0 = 0
  | otherwise = atan2 y x

-- | Gets the vector perpendicular to the given 2D vector.  If you pass it a vector
-- that is in a clockwise direction around a polygon, the result will always face
-- away from the polygon.
perpendicular2 :: Num a => Rel2' a -> Rel2' a
perpendicular2 (Rel2 (x,y) m) = Rel2 (-y, x) m

-- | Reflects the first direction vector against the given surface normal. The
-- resulting direction vector should have the same magnitude as the original
-- first parameter.  An example:
--
-- > makeRel2 (-3, -4) `reflectAgainst2` makeRel2 (0,1) == makeRel2 (-3, 4)
reflectAgainst2 :: (Floating a, Ord a) => Rel2' a -> Rel2' a -> Rel2' a
reflectAgainst2 v n = alongNormal + alongSurface
  where
    n' = unitVector n
    alongNormal = fmapNum1 (*(negate (v `projectOnto` n'))) n'
    alongSurface = fmapNum1 (*(v `projectOnto` perpendicular2 n')) (perpendicular2 n')

-- | Reflects the first direction vector against the given surface normal.  The
-- resulting direction vector should have the same magnitude as the original first
-- parameter.
-- 
-- The reflection is not performed if the given vector points along the same
-- direction as the normal, that is: if once projected onto the normal vector,
-- the component is positive, the original first parameter is returned
-- unmodified.  Examples:
--
-- > makeRel2 (-3, -4) `reflectAgainstIfNeeded2` makeRel2 (0,1) == makeRel2 (-3, 4)
-- > makeRel2 (-3, 4) `reflectAgainstIfNeeded2` makeRel2 (0,1) == makeRel2 (-3, 4)
reflectAgainstIfNeeded2 :: (Floating a, Ord a) => Rel2' a -> Rel2' a -> Rel2' a
reflectAgainstIfNeeded2 v n
  | towardsComponent < 0 = alongNormal + alongSurface
  | otherwise = v
  where
    n' = unitVector n
    towardsComponent = v `projectOnto` n'
    alongNormal = fmapNum1 (*(negate towardsComponent)) n'
    alongSurface = fmapNum1 (*(v `projectOnto` perpendicular2 n')) (perpendicular2 n')

-- | A line in 2D space.  A line is a point, and a free vector indicating
--  direction.  A line may be treated by a function as either finite (taking
--  the magnitude of the free vector as the length) or infinite (ignoring the
--  magnitude of the direction vector).
data Line2' a = Line2 {getLineStart2 :: (Point2' a) , getLineDir2 :: (Rel2' a)}
  deriving (Eq, Show, Read)

-- Given vectors: (x,y) + t(xd,yd)
--                (x',y') + t'(xd',yd')
-- Intersection is:
--
-- (x,y) + t(xd,yd) = (x',y') + t'(xd',yd')
--
-- Split, work with them in pairs:
--
-- (X1) x + t xd = x' + t' xd'
-- (Y1) y + t yd = y' + t' yd'
--
-- (X2a) t xd = x' + t' xd' - x
-- (Y2a) t yd = y' + t' yd' - y
--
-- (X3a) t xd yd = yd (x' + t' xd' - x)
-- (Y3a) t yd xd = xd (y' + t' yd' - y)
--
-- Now set RHSs equal:
-- 
-- (A1) yd (x' + t' xd' - x) = xd (y' + t' yd' - y)
-- (A2) yd (x' - x) + t' xd' yd = xd (y' - y) + t' xd yd'
-- (A3) t' xd' yd - t' xd yd' = xd (y' - y) - yd (x' - x)
-- (A4) t' (xd' yd - xd yd') = xd (y' - y) - yd (x' - x)
--
-- If (xd' yd - xd yd') /= 0:
-- t' = [xd (y' - y) - yd (x' - x)] / (xd' yd - xd yd')
--
-- Similarly:
-- (X2b) t' xd' = x + t xd - x'
-- (Y2b) t' yd' = y + t yd - y'
--
-- (X3b) t' xd' yd' = yd' (x + t xd - x')
-- (Y3b) t' yd' xd' = xd' (y + t yd - y')
--
-- Now set RHSs equal:
-- 
-- (B1) yd' (x + t xd - x') = xd' (y + t yd - y')
-- (B2) yd' (x - x') + t xd yd' = xd' (y - y') + t xd' yd
-- (B3) t xd yd' - t xd' yd = xd' (y - y') - yd' (x - x')
-- (B4) t (xd yd' - xd' yd) = xd' (y - y') - yd' (x - x')
--
-- If (xd yd' - xd' yd) /= 0 (note: negation of previous item)
-- t = [xd' (y - y') - yd' (x - x')] / (xd yd' - xd' yd)

-- | Given two 2D lines, finds out their intersection.  The first part of the
-- result pair is how much to multiply the direction vector of the first line
-- by (and add it to the start point of the first line) to reach the
-- intersection, and the second part is the corresponding item for the second line.
--  So given @Just (a, b) = intersectLines2 la lb@, it should be the case (minus
-- some possible precision loss) that @alongLine a la == alongLine b lb@.  If the
-- lines are parallel, Nothing is returned.
--
-- Note that this function assumes the lines are infinite.  If you want to check
-- for the intersection of two finite lines, check if the two parts of the result
-- pair are both in the range 0 to 1 inclusive.
intersectLines2 :: Fractional a => Line2' a -> Line2' a -> Maybe (a, a)
intersectLines2 (Line2 (Point2 (x,y)) (Rel2 (xd,yd) _)) (Line2 (Point2 (x',y')) (Rel2 (xd',yd') _))
  | a == 0 = Nothing
  | otherwise = Just $ (t, t')
  where
    a = (xd' * yd) - (xd * yd')
    t' = ((xd * (y' - y)) - (yd * (x' - x))) / a
    t = ((xd' * (y - y')) - (yd' * (x - x'))) / (negate a)

-- | Finds all the intersections between a line from the first list and a line from
-- the second list, and how far along that is each line.  That is, this is a bit
-- like mapMaybe composed with intersectLines2 on all pairings of a line from the
-- first list and a line from the second list.
findAllIntersections2 :: Fractional a => ([Line2' a], [Line2' a]) -> [((Line2' a, a), (Line2' a, a))]
findAllIntersections2 (as, bs)
  = catMaybes [ case intersectLines2 a b of
                  Just (ad, bd) -> Just ((a,ad), (b,bd))
                  Nothing -> Nothing
    | a <- as, b <- bs]

-- Vector: (x,y) = (x',y') + t(xd,yd)
-- Circle: (x-a)^2+(y-b)^2 = r^2
--
-- Substitute:
-- (x' + t xd - a)^2 + (y' + t yd - b)^2 = r^2
-- Define c = x' - a, d = y' - b:
-- (c + t xd)^2 + (d + t yd)^2 = r^2
-- t^2 (xd^2 + yd^2) + 2 (c xd + d yd) t + c^2 + d^2 - r^2 = 0
-- Then use quadratic formula!
--
-- We can take a slight short cut since xd^2 + yd^2 is the magnitude squared of
-- (xd, yd)
--
-- No ordering is guaranteed about the return values!


-- | Given a line, and a circle (defined by a point and a radius), finds the points
-- of intersection.
--
-- If the line does not intersect the circle, Nothing is returned.  If they do
-- intersect, two values are returned that are distances along the line.  That
-- is, given @Just (a, b) = intersectLineCircle l c@, the two points of intersection
-- are @(alongLine l a, alongLine l b)@.
--
-- The ordering of the two items in the pair is arbitrary, and if the line is a
-- tangent to the circle, the values will be the same.
intersectLineCircle :: (Ord a, Floating a) => Line2' a -> (Point2' a, a) -> Maybe (a, a)
intersectLineCircle (Line2 (Point2 (lx, ly)) (Rel2 (xd, yd) m))
                    (Point2 (cx, cy), r)
  = case b*b - 4*a*c of
      z | z < 0 -> Nothing
        | a == 0 -> -- all-zero direction vector
          if c == 0 -- If c is zero, the start point is on the line
            then Just (0,0)
            else Nothing
        | otherwise -> Just ((-b + sqrt z) / (2*a), (-b - sqrt z) / (2*a))
    where
      a = m
      b = 2 * ((lx - cx) * xd + (ly - cy) * yd)
      c = (lx - cx)*(lx - cx) + (ly - cy)*(ly - cy) - r*r

-- | Like 'pointAtZ', but returns a 2D vector instead of a 3D vector
point2AtZ :: (Geometry rel pt ln, Coord3 rel, Coord3 pt, Fractional a)
  => ln a -> a -> Maybe (Point2' a)
point2AtZ l = fmap (Point2 . (getX &&& getY) . flip alongLine l) . valueAtZ l
