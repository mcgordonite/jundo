-- | Experimental Event APIs not supported by the purescript-dom library
module DOM.Event.Experimental (
  fullscreenChange,
  pointerLockChange
  ) where

import Prelude
import DOM.Event.Types (EventType(..))

foreign import fullscreenChangeName :: String
foreign import pointerLockChangeName :: String

-- | Event type for fullscreenchange DOM events
fullscreenChange :: EventType
fullscreenChange = EventType fullscreenChangeName

-- | Event type for pointerlockchange DOM events
pointerLockChange :: EventType
pointerLockChange = EventType pointerLockChangeName
