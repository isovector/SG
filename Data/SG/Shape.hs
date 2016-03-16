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


-- | This module has types and functions for dealing with collision detection on
-- simple 2D shapes.
module Data.SG.Shape (Shape'(..), moveShape, rotateShape, scaleShape, shapePoints,
  boundingBox, overlap, intersectLineShape) where

import Control.Arrow
import Data.List
import Data.Maybe

import Data.SG.Geometry
import Data.SG.Geometry.TwoDim
import Data.SG.Matrix
import Data.SG.Vector

-- | A type for simple 2D convex shapes.  It is expected that you will define a
-- synonym in your own application such as @type Shape = Shape' Double@, hence
-- the funny name.
data Shape' a
       = Rectangle {shapeCentre :: Point2' a, rectSize :: (a, a)}
         -- ^ A rectangle with a centre, and a width (distance from the centre
         -- to the left or right side of the rectangle) and a height (distance
         -- from the centre to the top or bottom side of the rectangle.  So the
         -- rectangle with corners (1,1) and (3,2) is @Rectangle (Point2 (2,1.5))
         -- (1, 0.5)@.  Technically a rectangle is a polygon, of course, but a
         -- rectangle (which is axis-aligned) can be processed faster by most algorithms.
       | Circle {shapeCentre :: Point2' a, circSize :: a}
         -- ^ A circle with a centre and a radius.
       | Polygon {shapeCentre :: Point2' a,
           -- Points are offsets from centre (and join in loop):
           polyPoints :: [Rel2' a]}
         -- ^ A polygon with a centre, and a list of points.  The points are relative
         -- vectors from the centre of the polygon, and are expected to be in clockwise
         -- order.  For example, the triangle with corners (1,1) (3,3) and (3,1)
         -- could be @Polygon (Point2 (2.5, 1.5)) [Rel2 (-1.5,-0.5), Rel2 (0.5,1.5),
         -- Rel2 (-1.5, 1.5)]@.
         --
         -- Note that whereabouts the centre is inside the polygon is up to you
         -- (it does not /have to be/ the geometric average of the points), but
         -- it should at least be inside the polygon, or else some algorithms will
         -- behave strangely with it.
         --
         -- The list of points should have at least 3 points in it, or else some
         -- algorithms will behave strangely.
         --
         -- If your points are not in clockwise order (with the X-Y axes being
         -- how they are in graphs, not on screens), funny things will happen with
         -- the collision detection.
       deriving (Show, Read, Eq, Ord)

-- | Moves a shape by a given vector (by moving the centre).
moveShape :: Num a => Rel2' a -> Shape' a -> Shape' a
moveShape x s = s {shapeCentre = shapeCentre s `plusDir` x}

-- | Given an angle in /radians/, rotates the shape by that angle in an anti-clockwise
-- direction.  A circle will remain untouched, a polygon will have its points rotated,
-- and a rectangle will become a polygon and get rotated (even if you pass 0 as the angle).
rotateShape :: forall a. Floating a => a -> Shape' a -> Shape' a
rotateShape _ s@(Circle {}) = s
rotateShape a s@(Rectangle c _) = rotateShape a (Polygon c $ polygonPoints s)
rotateShape a (Polygon c ps) = Polygon c $ map (multMatrix mat) ps
  where
    mat :: Matrix22' a
    mat = rotateZaxis a

-- | Scales the size of the shape (for all edges, from the centre) by the given
-- factor.
scaleShape :: Num a => a -> Shape' a -> Shape' a
scaleShape a (Circle c r) = Circle c (r*a)
scaleShape a (Rectangle c (w, h)) = Rectangle c (w*a, h*a)
scaleShape a (Polygon c ps) = Polygon c $ map (scaleRel a) ps

pts :: Num a => Point2' a -> (a, a) -> (Point2' a, Point2' a)
pts (Point2 (x, y)) (adjX, adjY) = (Point2 (x - adjX, y - adjY), Point2 (x + adjX, y + adjY))

-- | Gives back the bounding box of a shape in terms of the minimum X-Y and
-- the maximum X-Y corners of the bounding box.
boundingBox :: (Num a, Ord a) => Shape' a -> (Point2' a, Point2' a)
boundingBox (Circle c r) = pts c (r, r)
boundingBox (Rectangle c (w, h)) = pts c (w, h)
boundingBox (Polygon p ps)
  = (p `plusDir` foldl (fmapNum2 min) (simpleVec 0) ps
    ,p `plusDir` foldl (fmapNum2 max) (simpleVec 0) ps)

twoFromList :: [a] -> Maybe (a, a)
twoFromList [] = Nothing
twoFromList [x] = Just (x, x)
twoFromList (x:y:_) = Just (x, y)

between :: Ord a => (a, a) -> a -> Bool
between (l, h) x = l <= x && x <= h

-- | Given a line and a shape, finds all possible intersections of the line
-- with the shape.  Since the shapes are convex, continuous 2D shapes, there
-- will either be no intersections or two (which could be the same point).
-- The returned value is distance along the line in multiples of the direction
-- vector (the return value is the same idea as 'intersectLineCircle').
intersectLineShape :: forall a. (Floating a, Ord a) => Line2' a -> Shape' a -> Maybe (a, a)
-- For circle, use existing function:
intersectLineShape l (Circle c r) = intersectLineCircle l (c, r)
-- For rectangle, use axis alignment:
intersectLineShape l (Rectangle (Point2 (x,y)) (w, h))
  = let leftE = fmap (flip alongLine l &&& id) $ valueAtX l (x-w)
        rightE = fmap (flip alongLine l &&& id) $ valueAtX l (x+w)
        topE = fmap (flip alongLine l &&& id) $ valueAtY l (y-h)
        bottomE = fmap (flip alongLine l &&& id) $ valueAtY l (y+h)
    in twoFromList $ map snd $
         (filter (between (y-h, y+h) . getY . fst) $ catMaybes [leftE, rightE])
         ++ (filter (between (x-w, x+w) . getX . fst) $ catMaybes [topE, bottomE])
-- For polygons, treat the line as a 0-length item in the perpendicular direction;
-- project all the polygon points onto that direction, and any that cross the 0-point
-- intersect.
intersectLineShape l (Polygon c ps)
  = twoFromList $ mapMaybe check $ pairsInLoop ps'
  where
    -- To translate points to the line, we must add the centre of the polygon,
    -- and subtract the start of the line:
    translate = (fmapNum2 (-) c (getLineStart l) `plusDir`)
    
    ps' = map (flip projectPointOnto2 $ id &&& perpendicular2 $ getLineDir l)
      $ map translate ps

    sc = mag $ getLineDir l

    check :: (Point2' a, Point2' a) -> Maybe a
    check (p@(Point2 (_, y)), p'@(Point2 (_, y')))
      = if signum y /= signum y'
          then fmap ((/ sc) . getX) $ pointAtY (p `lineTo` p') 0
          else Nothing

-- | Checks for overlap between the two shapes.  If they do not collide,
-- returns Nothing.  If they do collide, gives back suggested angles away from
-- each other.  These are not necessarily the shortest direction to separate
-- the two shapes, but should be decent for doing collision resolution (by using
-- them as surface normals, or push-away vectors)
--
-- The first vector returned is the direction in which the first shape should
-- head (or the surface normal to bounce the first shape off), whereas the
-- second vector returned is the direction in which the second shape should
-- head (or the surface normal to bounce the second shape off).
--
-- This function includes an initial quick test, followed by a more detailed test
-- if necessary.
overlap :: (Floating a, Ord a) => Shape' a -> Shape' a -> Maybe (Rel2' a, Rel2' a)
overlap a b
  | not (possibleOverlap a b) = Nothing
  | otherwise = detailedOverlap a b

-- | A quick test for possible intersection.
--
-- If it returns False, there is definitely no overlap.  If it returns True, there
-- might be some overlap.  For two circles, radiuses are checked (and the answer is
-- always accurate), for any other combination of shapes it checks bounding boxes.
possibleOverlap :: (Floating a, Ord a) => Shape' a -> Shape' a -> Bool
possibleOverlap (Circle ca ra) (Circle cb rb)
  = magSq (ca `fromPt` cb) <= ((ra+rb)*(ra+rb))
possibleOverlap a b
  = not $ don'tOverlap getX || don'tOverlap getY
  where
    (a1, a2) = boundingBox a
    (b1, b2) = boundingBox b
    don'tOverlap f = f a2 < f b1 || f a1 > f b2

-- Projects an already-moved shape onto that axis.  Returns a list of pairs where
-- each item of the pair also has an index for that point (for circles, this is
-- always -1).
projectShape :: (Ord a, Floating a) => Shape' a -> Rel2' a -> [(Int, a)]
projectShape (Circle c r) axis
  = let a = c `projectPointOnto` axis in [(-1,a - r), (-1, a + r)]
-- I am assuming (perhaps incorrectly) that projecting each point onto the axis
-- will be sufficient (rather than projecting each side)
projectShape (Polygon c ps) axis
  = zip [0..] $ map (((c `projectPointOnto` axis') +) . (`projectOnto` axis')) ps
  where axis' = unitVector axis
-- A rectangle has four points, all permutations of (+-w, +-h)
-- Projection is done using the dot product.  We can speed things up by calculating
-- the two components of the dot product once, then adding them in different ways
-- to achieve the projection.
projectShape (Rectangle c (w,h)) axis
  = zip [0..] $ map ((c `projectPointOnto` axis) +) [-dotx+doty,dotx+doty,dotx-doty,-dotx-doty]
  where
    dotx = w * getX (unitVector axis)
    doty = h * getY (unitVector axis)

-- All adjacent pairings, including last-first
pairsInLoop :: [a] -> [(a,a)]
pairsInLoop [] = []
pairsInLoop [_] = []
pairsInLoop xs = pairs' xs
  where
    -- all patterns are taken care of, despite what GHC thinks
    pairs' [x] = [(x, head xs)]
    pairs' (x:y:ys) = (x, y) : pairs' (y:ys)
    pairs' _ = error "Unreachable code in pairsInLoop in Shape module"

-- | Collects a list of (unit-vector) axes perpendicular to all the edges of the
-- polygon, pointed outwards.  The list will be empty for circles.
collectAxes :: (Floating a, Ord a) => Shape' a -> [Rel2' a]
collectAxes (Circle {}) = []
collectAxes (Polygon _ ps) = map unitVector [perpendicular2 (a + b) | (a,b) <- pairsInLoop ps]
collectAxes (Rectangle {}) = map (flip Rel2 1) [(-1,0), (1,0), (0, -1), (0, 1)]

-- | Given a shape, gets a list of relative vectors from the centre of the shape
-- to the points of the shape.  For polygons, this is the points list (unmodified).
--  For rectangles, it will be vectors to the four corners, and for circles, the
-- list will be empty.
polygonPoints :: Num a => Shape' a -> [Rel2' a]
polygonPoints (Circle {}) = []
polygonPoints (Rectangle _ (w, h))
  = map (flip Rel2 $ w*w + h*h) [(-w,h), (w, h), (w, -h), (-w, -h)]
polygonPoints (Polygon _ ps) = ps

-- | Given a shape, gets a list of points that make up the vertices of the
-- shape.  For circles, this list will be empty.
shapePoints :: Num a => Shape' a -> [Point2' a]
shapePoints s = map (shapeCentre s `plusDir`) (polygonPoints s)

-- | Gets a list of lines representing each side of the shape (headed clockwise).
--  For circles, the list will be empty.
polygonLines :: (Floating a) => Shape' a -> [Line2' a]
polygonLines s
  = map (uncurry lineTo)
      . pairsInLoop . map (shapeCentre s `plusDir`)
        . polygonPoints $ s
        
-- Gives back the reflected unit vector for each shape's angle away from the other.
-- returns Nothing if there was no collision after all.  You should only call this
-- if quickOverlap returned True.
detailedOverlap :: forall a. (Num a, Ord a, Floating a) => Shape' a -> Shape' a -> Maybe (Rel2' a, Rel2' a)
detailedOverlap (Circle pa _) (Circle pb _)
-- Rely on quickOverlap having been called:
  = let a_min_b = pa `fromPt` pb in Just (unitVector a_min_b, unitVector $ negate a_min_b)
-- We actually need to handle circle vs something, different than two polygons,
-- because a circle and polygon can intersect without points being contained inside
-- the other, which screws up our angle of incidence tests and so on.
--
-- We test which lines intersect the circle, and use those to form the angle of
-- incidence for the circle.  For the reverse, we just use the vector from the
-- centre of the circle to the average of the line intersections
detailedOverlap (Circle pa ra) pb
  | null intersections = Nothing
  | otherwise = Just ({- Angle from polygon -}
                 averageUnitVec $ map (perpendicular2 . getLineDir . fst) intersections
                ,{- Angle from circle -}
                 averageUnitVec $ map (`fromPt` pa)
                   $ map (uncurry $ flip alongLine) intersections
                )
  where
    intersections = filter (\(_,x) -> 0 <= x && x <= 1)
      $ concat [if a == b then [(l, a)] else [(l, a),(l, b)]
               | (l, Just (a,b)) <- map (id &&& flip intersectLineCircle (pa, ra))
          $ polygonLines pb]

detailedOverlap pa pb@(Circle {}) = fmap (\(x,y) -> (y,x)) $ detailedOverlap pb pa
-- Must be no circles now:
detailedOverlap pa pb
  = case foldl1 intersect' (map (uncurry getOverlaps) projected) of
      (aps, bps) | null aps && null bps -> Nothing
                 | otherwise ->
                    let aLines = getLineIndexes (length $ collectAxes pa) aps
                        bLines = getLineIndexes (length $ collectAxes pb) bps
                    in Just $ averageUnitVec *** averageUnitVec
                        $ unzip $ map (getPerpUnit *** getPerpUnit) $
                        map (fst *** fst) $ filter inBound $ findAllIntersections2
                          (map (polygonLines pb !!) bLines, map (polygonLines pa !!) aLines)
      
  where
    axes = collectAxes pa ++ collectAxes pb

    getPerpUnit = unitVector . perpendicular2 . (\(Line2 _ dir) -> dir)

    inBound ((_, ad), (_, bd)) = 0 <= ad && ad <= 1 && 0 <= bd && bd <= 1

    -- Given number of points, and some point indexes, gets the indexes of all
    -- the lines adjacent to those points.  If an empty list is given for the points,
    -- all line indexes are returned.
    getLineIndexes :: Int -> [Int] -> [Int]
    getLineIndexes total [] = [0 .. total - 1]
    getLineIndexes total ns = nub $ map (`mod` total) $ concatMap (\n -> [n-1,n]) ns

    projected :: [([(Int, a)], [(Int, a)])]
    projected = map (projectShape pa &&& projectShape pb) axes

    -- We can shortcut if any pair of lists involved is empty:
    intersect' :: ([Int], [Int]) -> ([Int], [Int]) -> ([Int], [Int])
    intersect' (as, bs) (cs, ds)
      | (null as && null bs) || (null cs && null ds) = ([], [])
      | otherwise = (as `intersect` cs, bs `intersect` ds)
    

    getOverlaps :: [(Int, a)] -> [(Int, a)] -> ([Int], [Int])
    getOverlaps as bs
      | maxa < minb || mina > maxb = ([], [])
      | otherwise = (map fst $ filter (overlapb . snd) as
                    ,map fst $ filter (overlapa . snd) bs)
      where
        getMinMax = minimum &&& maximum

        (mina, maxa) = getMinMax $ map snd as
        (minb, maxb) = getMinMax $ map snd bs
        overlapa x = mina <= x && x <= maxa
        overlapb x = minb <= x && x <= maxb
