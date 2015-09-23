-- | WebGL shader functions written with the free monad interface
module Graphics.WebGL.Free.Shaders (
  FragmentShader(..),
  VertexShader(..),
  buildFragmentShader,
  buildVertexShader,
  buildProgram,
  compileAndLinkProgram
  ) where

import Prelude
import Data.Either
import Data.Maybe
import Graphics.WebGL.Free
import qualified Graphics.WebGL.Raw.Enums as GL
import Graphics.WebGL.Raw.Types

-- | A type representing a WebGL fragment shader, since they are different to vertex shaders
newtype FragmentShader = FragmentShader WebGLShader

-- | A type representing a WebGL vertex shader, since they are different to fragment shaders
newtype VertexShader = VertexShader WebGLShader

buildShader :: DOMString -> GLenum-> WebGL (Either String WebGLShader)
buildShader source shaderType = do
  -- TODO: This will return Nothing if the context is lost
  Just shader <- createShader shaderType
  shaderSource shader source
  compileShader shader
  compiled <- getShaderCompileStatus shader
  if compiled
    then return $ Right shader
    else do
      maybeLog <- getShaderInfoLog shader
      case maybeLog of
        Nothing -> return $ Left "Shader Compile Error"
        Just log -> return $ Left $ "Shader Compile Error: " ++ log

-- | Compile the given source string into a fragment shader program, potentially returning an error
buildFragmentShader :: DOMString -> WebGL (Either String FragmentShader)
buildFragmentShader source = do
  eitherShader <- buildShader source GL.fragmentShader
  return $ FragmentShader <$> eitherShader

-- | Compile the given source string into a vertex shader program, potentially returning an error
buildVertexShader :: DOMString -> WebGL (Either String VertexShader)
buildVertexShader source = do
  eitherShader <- buildShader source GL.vertexShader
  return $ VertexShader <$> eitherShader

-- | Link a vertex shader and a fragment shader into a shader program, potentially returning an error
buildProgram :: VertexShader -> FragmentShader -> WebGL (Either String WebGLProgram)
buildProgram (VertexShader vs) (FragmentShader fs) = do
  -- TODO: This will return Nothing if the context is lost
  Just program <- createProgram
  attachShader program vs
  attachShader program fs
  linkProgram program
  linked <- getProgramLinkStatus program
  if linked
    then return $ Right program
    else do
      maybeLog <- getProgramInfoLog program
      case maybeLog of
        Nothing -> return $ Left "Program Link Error"
        Just log -> return $ Left $ "Program Link Error: " ++ log

-- | Compile and link the vertex and fragment shader source code into a shader program, potentially returning an error
compileAndLinkProgram :: DOMString -> DOMString -> WebGL (Either String WebGLProgram)
compileAndLinkProgram vertexSource fragmentSource = do
  eitherVS <- buildVertexShader vertexSource
  case eitherVS of
    Left err -> return $ Left err
    Right vs -> do
      eitherFS <- buildFragmentShader fragmentSource
      case eitherFS of
        Left err -> return $ Left err
        Right fs -> buildProgram vs fs
