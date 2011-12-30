--
module Geometry.Sphere
( volume
, area
) where

volume :: Float -> Float
volume radius = (4.0/3.0) * pi * (radius ^ 3)

area :: Float -> Float
area radius = 4 * pi * (radius ^ 2)

--
module Geometry.Cuboid
( volume
, area
) where

volume :: Float -> Float -> Float -> Float
volume a b c = a * b * c

area :: Float -> Float -> Float
area a b c = (2 * a * b) + (2 * a * c) + (2 * b * c)

--
module Geometry.Cube
( volume
, area
) where

import qualified Geometry.Cuboid as Cuboid

volume :: Float -> Float
volume a = Cuboid.volume a a a

area :: Float -> Float -> Float
area a = Cuboid.area a a a
