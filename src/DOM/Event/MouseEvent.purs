module DOM.Event.MouseEvent (
	MouseEventType(),
	addMouseEventListener,
	click,
	movementX,
	movementY
	) where

import Prelude
import Control.Monad.Eff
import DOM
import DOM.Event.Types
import qualified DOM.Event.EventTypes as D

data MouseEventType = MouseEventType EventType
click = MouseEventType D.click
mousemove = MouseEventType D.mousemove

foreign import movementX :: MouseEvent -> Number
foreign import movementY :: MouseEvent -> Number

foreign import data MouseEventListener :: # ! -> *
foreign import mouseEventListener :: forall eff a. (MouseEvent -> Eff eff a) -> MouseEventListener eff
foreign import addMouseEventListenerImpl :: forall eff. EventType -> MouseEventListener (dom :: DOM | eff) -> Boolean -> EventTarget -> Eff (dom :: DOM | eff) Unit

addMouseEventListener :: forall eff a. MouseEventType -> (MouseEvent -> Eff (dom :: DOM | eff) a) -> Boolean -> EventTarget -> Eff (dom :: DOM | eff) Unit
addMouseEventListener (MouseEventType event) callback useCapture target =
	addMouseEventListenerImpl event (mouseEventListener callback) useCapture target
