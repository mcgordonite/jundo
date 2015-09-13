-- Extra canvas stuff that (I think) is missing from the purescript-canvas package
module Graphics.Canvas.Extra where

import Control.Monad.Eff
import Graphics.Canvas

-- | Get the inner width of the canvas element
foreign import clientWidth :: forall eff. CanvasElement -> Eff (canvas :: Canvas | eff) Number

-- | Get the inner height of the canvas element
foreign import clientHeight :: forall eff. CanvasElement -> Eff (canvas :: Canvas | eff) Number
