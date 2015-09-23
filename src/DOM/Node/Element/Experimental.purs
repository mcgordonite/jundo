-- | Experimental Element APIs not supported by the purescript-dom library
module DOM.Node.Element.Experimental where

import Prelude
import Control.Monad.Eff
import DOM
import DOM.Node.Types

-- | Get the inner width in pixels of the given element
foreign import clientWidth :: forall eff. Element -> Eff (dom :: DOM | eff) Int

-- | Get the inner height in pixels of the given element
foreign import clientHeight :: forall eff. Element -> Eff (dom :: DOM | eff) Int

-- | Asynchronously request that the element be displayed in full screen
foreign import requestFullscreen :: forall eff. Element -> Eff (dom :: DOM | eff) Unit

-- Asynchronously request that the element be given pointer lock
foreign import requestPointerLock :: forall eff. Element -> Eff (dom :: DOM | eff) Unit
