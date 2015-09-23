-- | Extra WebGL stuff that is (I think) missing from the purescript-webgl-raw package
module Graphics.WebGL.Raw.Extra (
  bufferInt8Data,
  bufferInt16Data,
  bufferInt32Data,
  bufferUint8Data,
  bufferUint16Data,
  bufferUint32Data,
  bufferUint8ClampedData,
  bufferFloat32Data,
  bufferFloat64Data,
  getCanvas,
  getDrawingBufferHeight,
  getDrawingBufferWidth
  ) where

import Prelude
import Control.Monad.Eff
import qualified Data.ArrayBuffer.Types as A
import Graphics.Canvas (Canvas(), CanvasElement())
import Graphics.WebGL.Raw.Types

-- | Get the canvas to which the context is attached
foreign import getCanvas :: forall eff. WebGLContext -> Eff (canvas :: Canvas | eff) CanvasElement

-- | Get the drawing buffer width for the context
foreign import getDrawingBufferWidth :: forall eff. WebGLContext -> Eff (canvas :: Canvas | eff) Int

-- | Get the drawing buffer height for the context
foreign import getDrawingBufferHeight :: forall eff. WebGLContext -> Eff (canvas :: Canvas | eff) Int

-- A buffer data is present in the purescript-webgl-raw package but is limited to Float32Arrays
foreign import bufferData :: forall eff a. WebGLContext -> GLenum -> A.ArrayView a -> GLenum -> Eff (canvas :: Canvas | eff) Unit

-- | Create a buffer in memory and initialize it with data from an Int8Array
bufferInt8Data :: forall eff. WebGLContext -> GLenum -> A.Int8Array -> GLenum -> Eff (canvas :: Canvas | eff) Unit
bufferInt8Data = bufferData

-- | Create a buffer in memory and initialize it with data from an Int16Array
bufferInt16Data :: forall eff. WebGLContext -> GLenum -> A.Int16Array -> GLenum -> Eff (canvas :: Canvas | eff) Unit
bufferInt16Data = bufferData

-- | Create a buffer in memory and initialize it with data from an Int32Array
bufferInt32Data :: forall eff. WebGLContext -> GLenum -> A.Int32Array -> GLenum -> Eff (canvas :: Canvas | eff) Unit
bufferInt32Data = bufferData

-- | Create a buffer in memory and initialize it with data from an Uint8Array
bufferUint8Data :: forall eff. WebGLContext -> GLenum -> A.Uint8Array -> GLenum -> Eff (canvas :: Canvas | eff) Unit
bufferUint8Data = bufferData

-- | Create a buffer in memory and initialize it with data from an Uint16Array
bufferUint16Data :: forall eff. WebGLContext -> GLenum -> A.Uint16Array -> GLenum -> Eff (canvas :: Canvas | eff) Unit
bufferUint16Data = bufferData

-- | Create a buffer in memory and initialize it with data from an Uint32Array
bufferUint32Data :: forall eff. WebGLContext -> GLenum -> A.Uint32Array -> GLenum -> Eff (canvas :: Canvas | eff) Unit
bufferUint32Data = bufferData

-- | Create a buffer in memory and initialize it with data from an Uint8ClampedArray
bufferUint8ClampedData :: forall eff. WebGLContext -> GLenum -> A.Uint8ClampedArray -> GLenum -> Eff (canvas :: Canvas | eff) Unit
bufferUint8ClampedData = bufferData

-- | Create a buffer in memory and initialize it with data from an Float32Array
bufferFloat32Data :: forall eff. WebGLContext -> GLenum -> A.Float32Array -> GLenum -> Eff (canvas :: Canvas | eff) Unit
bufferFloat32Data = bufferData

-- | Create a buffer in memory and initialize it with data from an Float64Array
bufferFloat64Data :: forall eff. WebGLContext -> GLenum -> A.Float64Array -> GLenum -> Eff (canvas :: Canvas | eff) Unit
bufferFloat64Data = bufferData
