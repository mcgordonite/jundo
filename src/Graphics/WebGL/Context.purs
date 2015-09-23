-- | Functions for getting a WebGL context from a canvas element
module Graphics.WebGL.Context where

import Prelude
import Control.Monad.Eff
import Graphics.Canvas (Canvas(), CanvasElement())
import Graphics.WebGL.Raw.Types

foreign import getWebGLContextWithAttrs :: forall eff. CanvasElement -> WebGLContextAttributes -> Eff (canvas :: Canvas | eff) WebGLContext

-- | Default attributes for a WebGL context
defaultWebGLContextAttrs :: WebGLContextAttributes
defaultWebGLContextAttrs = {
  alpha: true,
  depth: true,
  stencil: false,
  antialias: true,
  premultipliedAlpha: true,
  preserveDrawingBuffer: false,
  preferLowPowerToHighPerformance: false,
  failIfMajorPerformanceCaveat: false
  }

-- | Get a WebGL context from a canvas element
getWebGLContext :: forall eff. CanvasElement -> Eff (canvas :: Canvas | eff) WebGLContext
getWebGLContext el = getWebGLContextWithAttrs el defaultWebGLContextAttrs
