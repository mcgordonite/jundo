module Test.Main where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console
import Data.Either
import Data.Array
import Data.Tuple
import Data.Vector
import Data.Vector2
import Data.Vector3
import Jundo.WavefrontObj
import Test.Assert

-- No vertex data has been defined so this should fail to parse
noVertexMesh :: String
noVertexMesh = "f 1/1/1 1/1/1 1/1/1"

triangleMesh :: String
triangleMesh = """
  # Triangle mesh
  o triangle_mesh.obj

  # Vertices
  v 0.5 0 0
  v 0 1 0
  v 0 0 0

  # Normals
  vn 0 0 1.0

  # Texture Coordinates
  vt 0.0 0.0
  vt 1.0 0.0

  # Faces
  f 1/1/1 2/1/1 3/2/1
  """

cubeMesh :: String
cubeMesh = """
  # Cube mesh
  o cube_mesh.obj
  v 1 1 1
  v -1 1 1
  v -1 -1 1
  v 1 -1 1
  v 1 1 -1
  v -1 1 -1
  v -1 -1 -1
  v 1 -1 -1
  vn 0 0 1
  vn 1 0 0
  vn -1 0 0
  vn 0 0 -1
  vn 0 1 0
  vn 0 -1 0
  vt 0.0 0.0
  vt 0.0 1.0
  vt 1.0 0.0
  vt 1.0 1.0
  f 1/1/1 2/2/1 3/3/1
  f 3/3/1 4/4/1 1/1/1
  f 5/1/2 1/2/2 4/3/2
  f 4/3/2 8/4/2 5/1/2
  f 2/1/3 6/2/3 7/3/3
  f 7/3/3 3/4/3 2/1/3
  f 7/1/4 8/2/4 5/3/4
  f 5/3/4 6/4/4 7/1/4
  f 5/1/5 6/2/5 2/3/5
  f 2/3/5 1/4/5 5/1/5
  f 8/1/6 4/2/6 3/3/6
  f 3/3/6 7/4/6 8/1/6
  """

assertRight :: forall eff a b. Either a b -> Eff (assert :: ASSERT, console :: CONSOLE | eff) Unit
assertRight = assert <<< isRight

assertLeft :: forall eff a b. Either a b -> Eff (assert :: ASSERT, console :: CONSOLE | eff) Unit
assertLeft = assert <<< isLeft

main :: Eff (assert :: ASSERT, console :: CONSOLE) Unit
main = do
  log "Parse mesh with no vertex data"
  assertLeft $ parseObj noVertexMesh

  log "\nParse triangle mesh"
  parsedTriangleMesh <- pure (parseObj triangleMesh)
  log "Right"
  assertRight parsedTriangleMesh
  case parsedTriangleMesh of
    Right (Mesh mesh) -> do
      log "Elements length 3"
      assert (length mesh.elements == 3)
      log "Elements equal [0, 1, 2]"
      assert (mesh.elements == [0, 1, 2])

      log "Vertices length 9"
      assert (length mesh.vertices == 9)
      log "Vertices equal [0.5, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0]"
      assert (mesh.vertices == [0.5, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0])

      log "Normals length 3"
      assert (length mesh.normals == 9)
      log "Normals equal [0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0]"

      log "UVS length 6"
      assert (length mesh.uvs == 6)
      log "UVS equal [0.0 0.0, 0.0 0.0, 1.0, 0.0]"
      assert (mesh.uvs == [0.0, 0.0, 0.0, 0.0, 1.0, 0.0])
    _ -> assert false

  log "\nParse cube mesh"
  parsedCubeMesh <- pure (parseObj cubeMesh)
  log "Right"
  assertRight parsedCubeMesh
  case parsedCubeMesh of
    Right (Mesh mesh) -> do
      log "Elements length 36"
      assert (length mesh.elements == 36)
    _ -> assert false
