module DOM.HTML.Window.Dimensions where

import Prelude
import DOM
import DOM.HTML.Types
import Control.Monad.Eff

foreign import innerWidth :: forall eff. Window -> Eff (dom :: DOM | eff) Number

foreign import innerHeight :: forall eff. Window -> Eff (dom :: DOM | eff) Number
