-- | Experimental Document APIs not supported by the purescript-dom library
module DOM.Node.Document.Experimental where

import Prelude
import Control.Monad.Eff
import Data.Nullable
import DOM
import DOM.Node.Types

-- | Get the element that's currently being displayed in full screen, or null if there isn't one
foreign import fullscreenElement :: forall eff. Document -> Eff (dom :: DOM | eff) (Nullable Element)

-- | Check if the document supports full screen mode
foreign import fullscreenEnabled :: forall eff. Document -> Eff (dom :: DOM | eff) Boolean

-- | Asynchronously exit pointer lock
foreign import exitPointerLock :: forall eff. Document -> Eff (dom :: DOM | eff) Unit

-- | Get the element that currently has a pointer lock, or null if there isn't one
foreign import pointerLockElement :: forall eff. Document -> Eff (dom :: DOM | eff) (Nullable Element)

