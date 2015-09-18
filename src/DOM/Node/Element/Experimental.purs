-- Experimental Element APIs not supported by the purescript-dom library
module DOM.Node.Element.Experimental where

import Prelude
import Control.Monad.Eff
import DOM
import DOM.Node.Types

foreign import clientWidth :: forall eff. Element -> Eff (dom :: DOM | eff) Int
foreign import clientHeight :: forall eff. Element -> Eff (dom :: DOM | eff) Int
foreign import requestFullscreen :: forall eff. Element -> Eff (dom :: DOM | eff) Unit
foreign import requestPointerLock :: forall eff. Element -> Eff (dom :: DOM | eff) Unit

