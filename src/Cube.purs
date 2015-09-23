-- | Logic for creating a cube in WebGL
module Cube (
  CubeBuffers(..),
  initialiseBuffers
  ) where

import Prelude
import Data.ArrayBuffer.Types (Float32Array(), Uint16Array())
import Data.Maybe
import Data.TypedArray (asFloat32Array, asUint16Array)
import Graphics.WebGL.Free
import qualified Graphics.WebGL.Raw.Enums as GL
import Graphics.WebGL.Raw.Types

type CubeBuffers = {vertex :: WebGLBuffer, index :: WebGLBuffer}

vertices :: Float32Array
vertices = asFloat32Array [
  -- Front face
  -1.0, -1.0, 1.0,
  1.0, -1.0, 1.0,
  1.0, 1.0, 1.0,
  -1.0, 1.0, 1.0,
  -- Back face
  -1.0, -1.0, -1.0,
  -1.0, 1.0, -1.0,
  1.0, 1.0, -1.0,
  1.0, -1.0, -1.0,
  -- Top face
  -1.0, 1.0, -1.0,
  -1.0, 1.0, 1.0,
  1.0, 1.0, 1.0,
  1.0, 1.0, -1.0,
  -- Bottom face
  -1.0, -1.0, -1.0,
  1.0, -1.0, -1.0,
  1.0, -1.0, 1.0,
  -1.0, -1.0, 1.0,
  -- Right face
  1.0, -1.0, -1.0,
  1.0, 1.0, -1.0,
  1.0, 1.0, 1.0,
  1.0, -1.0, 1.0,
  -- Left face
  -1.0, -1.0, -1.0,
  -1.0, -1.0, 1.0,
  -1.0, 1.0, 1.0,
  -1.0, 1.0, -1.0
  ]

vertexIndices :: Uint16Array
vertexIndices = asUint16Array [
  -- Front face
  0, 1, 2,
  0, 2, 3,
  -- Back face
  4, 5, 6,
  4, 6, 7,
  -- Top face
  8, 9, 10,
  8, 10, 11,
  -- Bottom face
  12, 13, 14,
  12, 14, 15,
  -- Right face
  16, 17, 18,
  16, 18, 19,
  -- Left face
  20, 21, 22,
  20, 22, 23
  ]

-- TODO: Create buffer will return Nothing if the context is lost
-- | Create and populate the WebGL buffers required to render a plain old cube
initialiseBuffers :: WebGL CubeBuffers
initialiseBuffers = do
  Just cubeVertexBuffer <- createBuffer
  bindBuffer GL.arrayBuffer cubeVertexBuffer
  bufferFloat32Data GL.arrayBuffer vertices GL.staticDraw

  Just cubeIndexBuffer <- createBuffer
  bindBuffer GL.elementArrayBuffer cubeIndexBuffer
  bufferUint16Data GL.elementArrayBuffer vertexIndices GL.staticDraw

  return {vertex: cubeVertexBuffer, index: cubeIndexBuffer}
