module DOM.Node.Document.Extra where

import Prelude
import Control.Monad.Eff
import Data.Nullable
import DOM
import DOM.Node.Types

foreign import fullscreenElement :: forall eff. Document -> Eff (dom :: DOM | eff) (Nullable Element)
foreign import fullscreenEnabled :: forall eff. Document -> Eff (dom :: DOM | eff) Boolean

