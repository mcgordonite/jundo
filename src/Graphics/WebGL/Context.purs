module Graphics.WebGL.Context where

import Prelude
import Control.Monad.Eff
import Graphics.Canvas (Canvas(), CanvasElement())
import Graphics.WebGL.Raw.Types

foreign import getWebGLContextWithAttrs :: CanvasElement -> WebGLContextAttributes -> Eff (canvas :: Canvas | eff) WebGLContext

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

getWebGLContext :: CanvasElement -> Eff (canvas :: Canvas | eff) WebGLContext
getWebGLContext el = getWebGLContextWithAttrs el defaultWebGLContextAttrs
