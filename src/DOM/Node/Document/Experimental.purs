-- Experimental Eocument APIs not supported by the purescript-dom library
module DOM.Node.Document.Experimental where

import Prelude
import Control.Monad.Eff
import Data.Nullable
import DOM
import DOM.Node.Types

foreign import fullscreenElement :: forall eff. Document -> Eff (dom :: DOM | eff) (Nullable Element)
foreign import fullscreenEnabled :: forall eff. Document -> Eff (dom :: DOM | eff) Boolean
foreign import exitPointerLock :: forall eff. Document -> Eff (dom :: DOM | eff) Unit
foreign import pointerLockElement :: forall eff. Document -> Eff (dom :: DOM | eff) (Nullable Element)
 