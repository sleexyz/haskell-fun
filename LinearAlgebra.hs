-- | Functional linear algebra

module LinAlg where

import Data.Profunctor
import Control.Applicative

-- | One perspective of functions are "vectors"
-- | and their types as "vector spaces"

-- | This is related to linear functionals:


-- | Scalar multiplication
(*^) :: Float -> (a -> Float) -> (a -> Float)
x *^ y = rmap (*x) y

type Vec1 = () -> Float      -- = Float^1
type Vec2 = Bool -> Float    -- = Float^2


-- | A simple vector
vec2 :: Bool -> Float
vec2 True = 1
vec2 False = 0

-- | Rotation matrix!
rot :: Float -> (Bool -> Float) -> (Bool -> Float)
rot phi v =
  let
    x = v True
    y = v False

    v' True = (cos phi * x) + negate (sin phi * y)
    v' False = (sin phi * x) + (cos phi * y)
  in
    v'


vec2' :: Bool -> Float
vec2' = rot (pi/2) vec2


-- | 180 degree rotation: a linear map
--
-- If we think of vectors as functions
-- Then our linear map is a vector in the vector space:
-- (Float^Bool)^(Float^Bool)
rot90:: (Bool -> Float) -> (Bool -> Float)
rot90 = rot (pi/2)


-- | This is isomorphic to the matrix representation
around'  :: (Bool, Bool) -> Float
around' (i, j)  = rot90 x i + rot90 y j
  where
    x True = 1
    x False = 0

    y True = 0
    y False = 1

