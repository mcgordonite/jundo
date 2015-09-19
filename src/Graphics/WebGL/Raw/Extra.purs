-- Extra WebGL stuff that is (I think) missing from the purescript-webgl-raw package
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

-- Buffer data is present in the purescript-webgl-raw package but is limited to Float32Arrays
foreign import bufferData :: forall eff a. WebGLContext -> GLenum -> A.ArrayView a -> GLenum -> Eff (canvas :: Canvas | eff) Unit

bufferInt8Data :: forall eff. WebGLContext -> GLenum -> A.Int8Array -> GLenum -> Eff (canvas :: Canvas | eff) Unit
bufferInt8Data = bufferData

bufferInt16Data :: forall eff. WebGLContext -> GLenum -> A.Int16Array -> GLenum -> Eff (canvas :: Canvas | eff) Unit
bufferInt16Data = bufferData

bufferInt32Data :: forall eff. WebGLContext -> GLenum -> A.Int32Array -> GLenum -> Eff (canvas :: Canvas | eff) Unit
bufferInt32Data = bufferData

bufferUint8Data :: forall eff. WebGLContext -> GLenum -> A.Uint8Array -> GLenum -> Eff (canvas :: Canvas | eff) Unit
bufferUint8Data = bufferData

bufferUint16Data :: forall eff. WebGLContext -> GLenum -> A.Uint16Array -> GLenum -> Eff (canvas :: Canvas | eff) Unit
bufferUint16Data = bufferData

bufferUint32Data :: forall eff. WebGLContext -> GLenum -> A.Uint32Array -> GLenum -> Eff (canvas :: Canvas | eff) Unit
bufferUint32Data = bufferData

bufferUint8ClampedData :: forall eff. WebGLContext -> GLenum -> A.Uint8ClampedArray -> GLenum -> Eff (canvas :: Canvas | eff) Unit
bufferUint8ClampedData = bufferData

bufferFloat32Data :: forall eff. WebGLContext -> GLenum -> A.Float32Array -> GLenum -> Eff (canvas :: Canvas | eff) Unit
bufferFloat32Data = bufferData

bufferFloat64Data :: forall eff. WebGLContext -> GLenum -> A.Float64Array -> GLenum -> Eff (canvas :: Canvas | eff) Unit
bufferFloat64Data = bufferData
