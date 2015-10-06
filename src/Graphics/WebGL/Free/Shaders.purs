-- | WebGL shader functions written with the free monad interface
module Graphics.WebGL.Free.Shaders (
  buildProgram,
  compileAndLinkProgram
  ) where

import Prelude
import Data.Either
import Data.Maybe
import Graphics.WebGL.Free

buildShader :: forall s. (WebGLTypedShader s) => DOMString -> WebGL (Either String s)
buildShader source = do
  -- TODO: This will return Nothing if the context is lost
  Just shader <- createShader
  setShaderSource shader source
  compileShader shader
  compiled <- getShaderCompileStatus shader
  if compiled
    then return $ Right shader
    else do
      maybeLog <- getShaderInfoLog shader
      case maybeLog of
        Nothing -> return $ Left "Shader Compile Error"
        Just log -> return $ Left $ "Shader Compile Error: " ++ log

-- | Link a vertex shader and a fragment shader into a shader program, potentially returning an error
buildProgram :: VertexShader -> FragmentShader -> WebGL (Either String WebGLProgram)
buildProgram vs fs = do
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
  eitherVS <- buildShader vertexSource
  case eitherVS of
    Left err -> return $ Left err
    Right vs -> do
      eitherFS <- buildShader fragmentSource
      case eitherFS of
        Left err -> return $ Left err
        Right fs -> buildProgram vs fs
