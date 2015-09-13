-- Extra WebGL stuff that is (I think) missing from the purescript-webgl-raw package
module Graphics.WebGL.Raw.Extra where

import Control.Monad.Eff
import Graphics.Canvas (Canvas(), CanvasElement())
import Graphics.WebGL.Raw.Types

-- | Get the canvas to which the context is attached
foreign import getCanvas :: forall eff. WebGLContext -> Eff (canvas :: Canvas | eff) CanvasElement

-- | Get the drawing buffer width for the context
foreign import getDrawingBufferWidth :: forall eff. WebGLContext -> Eff (canvas :: Canvas | eff) Int

-- | Get the drawing buffer height for the context
foreign import getDrawingBufferHeight :: forall eff. WebGLContext -> Eff (canvas :: Canvas | eff) Int
