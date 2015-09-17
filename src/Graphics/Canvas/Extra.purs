module Graphics.Canvas.Extra where

import DOM.Node.Types
import Graphics.Canvas (CanvasElement())
import Unsafe.Coerce

-- | Convert a canvas element to a DOM element
toElement :: CanvasElement -> Element
toElement = unsafeCoerce

