module DOM.Event.Extra (
	fullscreenChange
	) where

import Prelude
import DOM.Event.Types (EventType(..))

foreign import fullscreenChangeName :: String

fullscreenChange :: EventType
fullscreenChange = EventType fullscreenChangeName

