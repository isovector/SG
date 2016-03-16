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


-- | A small geometry library, with vectors, matrices and simple shape
-- collision detection that is intended to be straightforward in two and three
-- dimensions.
--
-- The basics of vectors are in the "Data.SG.Vector" module, the basics of lines
-- and geometry tests (e.g. testing whether a point is on a line) are in "Data.SG.Geometry",
-- with further specialised tests in "Data.SG.Geometry.TwoDim" and "Data.SG.Geometry.ThreeDim".
--  Matrix transformations are in "Data.SG.Matrix" and shapes (with collision detection)
-- are in "Data.SG.Shape".
--
-- The names for most of the types in this library end with a prime.  This is because
-- it is intended that you specialise these types (usually to Float or Double)
-- in your application as follows:
--
-- > type Point2 = Point2' Double
-- > type Rel2 = Rel2' Double
-- > type Line2 = Line2' Double
-- > type Matrix22 = Matrix22' Double
--
-- Much of the use of the types (especially vectors) in this library is made
-- using type-classes such as Num, Functor, Applicative and so on.  For more
-- explanation on some of the less well-known type-classes, see either the
-- article Typeclassopedia in The Monad Reader
-- (<http://www.haskell.org/haskellwiki/The_Monad.Reader>) issue 13
-- (<http://www.haskell.org/sitewiki/images/8/85/TMR-Issue13.pdf>), or my own notes
-- at <http://www.twistedsquare.com/haskell.html>.
--
-- To understand what various functions will actually do, look at the SGdemo project
-- (<http://hackage.haskell.org/cgi-bin/hackage-scripts/package/SGdemo>)
-- on Hackage (and its code) which provides a visual demonstration of several of
-- the functions.
module Data.SG
  (module Data.SG.Vector
  ,module Data.SG.Vector.Basic
  ,module Data.SG.Geometry
  ,module Data.SG.Geometry.TwoDim
  ,module Data.SG.Geometry.ThreeDim
  ,module Data.SG.Matrix
  ,module Data.SG.Shape
  ) where

import Data.SG.Vector
import Data.SG.Vector.Basic
import Data.SG.Geometry
import Data.SG.Geometry.TwoDim
import Data.SG.Geometry.ThreeDim
import Data.SG.Matrix
import Data.SG.Shape
