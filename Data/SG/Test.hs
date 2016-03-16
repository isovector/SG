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

-- | A test module (run main)
module Data.SG.Test where

import Control.Arrow
import qualified Data.List as List
import Test.HUnit

import Data.SG

newtype Float' = Float' Float deriving (Ord, Enum, Num, Fractional, Floating, Show)
newtype Double' = Double' Double deriving (Ord, Enum, Num, Fractional, Floating, Show)

instance Eq Float' where
  a == b = abs (a - b) < 0.001

instance Eq Double' where
  a == b = abs (a - b) < 0.001

class (Eq a, Floating a, Enum a, Ord a) => TestFloating a where
  input :: [a]
  input = [-10..10]

  angs :: [a]
  angs = map (*(50/pi)) [0..100]

  testItems2 :: ((a,a) -> b) -> [b]
  testItems2 f = [f (x, y) | x <- [-10..10], y <- [-10..10]]

  testItems3 :: ((a,a,a) -> b) -> [b]
  testItems3 f = [f (x, y, z) | x <- [-10..10], y <- [-10..10], z <- [-10..10]]

  forRot2 :: ((b, b) -> IO ()) -> (((a,a) -> (a,a)) -> [(b,b)]) -> IO ()
  forRot2 f g = mapM_ f (concatMap g $ map onPair ops)
    where
      ops :: [Pair a -> Pair a]
      ops = [\p -> multMatrix (rotate2D a) p | a <- angs]
      
      onPair f p = let Pair p' = f (Pair p) in p'

  forRot3 :: ((b, b) -> IO ()) -> (((a,a,a) -> (a,a,a)) -> [(b,b)]) -> IO ()
  forRot3 f g = mapM_ f (g id)

  test_mag2 :: a -> ((a, a) -> a) -> IO ()
  test_mag2 _ f = forRot2 (uncurry $ assertEqual "test_mag2")
    $ \rot -> [(sqrt $ (x*x) + (y*y), f (rot (x,y))) | x <- input, y <- input]

  test_unit2 :: (Eq (p a), Show (p a), VectorNum p, Coord p) =>
    a -> ((a, a) -> p a) -> IO ()
  test_unit2 _ f = forRot2 (uncurry $ assertEqual "test_unit2")
    $ \rot -> [let v = f (rot p) in (v, (fmapNum1 (* mag v) $ unitVector v)) | p <- testItems2 id]

  test_unit3 :: (Eq (p a), Show (p a), VectorNum p, Coord p) =>
    a -> ((a, a, a) -> p a) -> IO ()
  test_unit3 _ f = forRot3 (uncurry $ assertEqual "test_unit3")
    $ \rot -> [let v = f (rot p) in (v, (fmapNum1 (* mag v) $ unitVector v)) | p <- testItems3 id]

  testRotId :: (Eq (p a), Show (p a), IsomorphicVectors Pair p, IsomorphicVectors
    p Pair) => a -> ((a, a) -> p a) -> IO ()
  testRotId _ f = do mapM_ (uncurry $ assertEqual "testRotId")
                       $ map (id &&& (rotate2D 0 `multMatrix`)) $ testItems2 f
                     mapM_ (uncurry $ assertEqual "testRotId")
                       $ map (id &&& (rotate2D (2*pi) `multMatrix`)) $ testItems2 f
                     mapM_ (uncurry $ assertEqual "testRotId")
                       $ map (id &&& (rotate2D (4*pi) `multMatrix`)) $ testItems2 f
                     mapM_ (uncurry $ assertEqual "testRotId")
                       $ map (id &&& (rotate2D (-2*pi) `multMatrix`)) $ testItems2 f
                     mapM_ (uncurry $ assertEqual "testRotId-bothways")
                       [(rotate2D a `multMatrix` v
                        ,rotate2D (negate (2*pi - a)) `multMatrix` v
                        )
                       | v <- testItems2 f, a <- angs]
                     mapM_ (uncurry $ assertEqual "testRotId-twice")
                       [(v,rotate2D a `multMatrix` (rotate2D (2*pi - a) `multMatrix` v))
                       | v <- testItems2 f, a <- angs]

  testProject :: (VectorNum p, Coord p) => a -> ((a, a) -> p a) -> IO ()
  testProject _ f = do
    forRot2 (uncurry $ assertEqual "testProject") $
      \rot -> [ let r = f $ rot (0 :: a, 1)
                    v = f (rot p)
                in (snd p, v `projectOnto` r)
              | p <- testItems2 id]
    forRot2 (uncurry $ assertEqual "testProject") $
      \rot -> [ let r = f $ rot (0 :: a, -1)
                    v = f (rot p)
                in (negate $ snd p, v `projectOnto` r)
              | p <- testItems2 id]

  testReflect :: a -> IO ()
  testReflect _ = do
    forRot2 (uncurry $ assertEqual "testReflect0") $
      \rot -> [ let r = makeRel2 $ rot (0 :: a, 1)
                    v = makeRel2 $ rot p
                    v' = makeRel2 $ rot $ second negate p
                in (v', v `reflectAgainst2` r)
              | p <- testItems2 id]
    forRot2 (uncurry $ assertEqual "testReflect1") $
      \rot -> [ let r = makeRel2 $ rot (0 :: a, -1)
                    v = makeRel2 $ rot p
                    v' = makeRel2 $ rot $ second negate p
                in (v', v `reflectAgainst2` r)
              | p <- testItems2 id]
    forRot2 (uncurry $ assertEqual "testReflect2") $
      \rot -> [ let r = makeRel2 $ rot (0 :: a, 1)
                    v = makeRel2 $ rot p
                    v' = makeRel2 $ rot $ second (\x -> if x > 0 then x else negate x) p
                in (v', v `reflectAgainstIfNeeded2` r)
              | p <- testItems2 id]
    forRot2 (uncurry $ assertEqual "testReflect3") $
      \rot -> [ let r = makeRel2 $ rot (0 :: a, -1)
                    v = makeRel2 $ rot p
                    v' = makeRel2 $ rot $ second (\x -> if x < 0 then x else negate x) p
                in (v', v `reflectAgainstIfNeeded2` r)
              | p <- testItems2 id]

  testMatrixId :: (Matrix (SquareMatrix c), Num (SquareMatrix c a)) => SquareMatrix c a -> IO ()
  testMatrixId x
    = do let id = fromMatrixComponents [] `asTypeOf` x
             size = length (matrixComponents id)
             groupInto n xs = take n xs : groupInto n (drop n xs)
             ns = fromMatrixComponents $ groupInto size [1..]
         assertEqual "testMatrixId 0" id (transpose id)
         assertEqual "testMatrixId 1" id (id*id)
         assertEqual "testMatrixId 2" id (id*id*transpose id*id)
         assertEqual "testMatrixId 3" ns (transpose $ transpose $ ns)
         assertEqual "testMatrixId 4" (transpose ns)
           (fromMatrixComponents $ List.transpose $ matrixComponents $ ns)
         assertEqual "testMatrixId 5" ns (id * ns)
         assertEqual "testMatrixId 6" ns (ns * id)

  testAll :: a -> IO ()
  testAll x
        = do test_mag2 x (mag . makeRel2)
             test_mag2 x (mag . Point2)
             test_mag2 x (mag . Pair)
             test_unit2 x Point2
             test_unit2 x makeRel2
             test_unit2 x Pair
             test_unit3 x Point3
             test_unit3 x makeRel3
             test_unit3 x Triple
             testRotId x Point2
             testRotId x makeRel2
             testRotId x Pair
             testMatrixId (undefined :: Matrix22' a)
             testMatrixId (undefined :: Matrix33' a)
             testMatrixId (undefined :: Matrix44' a)
             testProject x Point2
             testProject x makeRel2
             testProject x Pair
             testReflect x


instance TestFloating Float'
instance TestFloating Double'

main :: IO ()
main = do testAll (0 :: Float')
          testAll (0 :: Double')
