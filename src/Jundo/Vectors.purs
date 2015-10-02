-- | Vector utility functions
module Jundo.Vectors where

import Prelude
import Data.Vector3
import Data.Vector4
import Data.Matrix4
import Math

-- | Takes the X, Y and Z values from a Vec4 and returns them as a Vec3
dropW :: forall a. Vec4 a -> Vec3 a
dropW v4 = vec3 (get4X v4) (get4Y v4) (get4Z v4)

-- | Add a w coordinate to a Vec3, creating a Vec4
withW :: forall a. Vec3 a -> a -> Vec4 a
withW v3 w = vec4 (get3X v3) (get3Y v3) (get3Y v3) w

-- | Rotate the vector the given number of radians around the given axis
rotateVec3 :: Vec3N -> Radians -> Vec3N -> Vec3N
rotateVec3 axis angle input = dropW $ mulMatVect (makeRotate angle axis) (withW input 0.0)
