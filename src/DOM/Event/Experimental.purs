-- | Experimental Event APIs not supported by the purescript-dom library
module DOM.Event.Experimental (
  fullscreenChange
  ) where

import Prelude
import DOM.Event.Types (EventType(..))

foreign import fullscreenChangeName :: String

-- | Event type for fullscreenchange DOM events
fullscreenChange :: EventType
fullscreenChange = EventType fullscreenChangeName
