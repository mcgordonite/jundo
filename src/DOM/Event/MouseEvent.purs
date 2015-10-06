-- | Functions relating to the MouseEvent type
module DOM.Event.MouseEvent (
  MouseEventType(),
  MouseEventListener(),
  mouseEventListener,
  addMouseEventListener,
  removeMouseEventListener,
  click,
  mousemove,
  movementX,
  movementY
  ) where

import Prelude
import Control.Monad.Eff
import DOM
import DOM.Event.Types
import qualified DOM.Event.EventTypes as D

-- | Type for mouse event names
newtype MouseEventType = MouseEventType EventType

click = MouseEventType D.click
mousemove = MouseEventType D.mousemove

-- | Get the movement in the x direction from a mouse event
foreign import movementX :: MouseEvent -> Number

-- | Get the movement in the y direction from a mouse event
foreign import movementY :: MouseEvent -> Number

-- | Type representing a DOM EventListener for MouseEvents
foreign import data MouseEventListener :: # ! -> *

--  | Create a MouseEventListener from an action in the Eff monad
foreign import mouseEventListener :: forall eff a. (MouseEvent -> Eff eff a) -> MouseEventListener eff

foreign import addMouseEventListenerImpl :: forall eff. EventType -> MouseEventListener (dom :: DOM | eff) -> Boolean -> EventTarget -> Eff (dom :: DOM | eff) Unit
foreign import removeMouseEventListenerImpl :: forall eff. EventType -> MouseEventListener (dom :: DOM | eff) -> Boolean -> EventTarget -> Eff (dom :: DOM | eff) Unit

-- | Register a MouseEventListener for the given event type and target
addMouseEventListener :: forall eff. MouseEventType -> MouseEventListener (dom :: DOM | eff) -> Boolean -> EventTarget -> Eff (dom :: DOM | eff) Unit
addMouseEventListener (MouseEventType eventType) = addMouseEventListenerImpl eventType

-- | Remove a MouseEventListener
removeMouseEventListener :: forall eff. MouseEventType -> MouseEventListener (dom :: DOM | eff) -> Boolean -> EventTarget -> Eff (dom :: DOM | eff) Unit
removeMouseEventListener (MouseEventType eventType) = removeMouseEventListenerImpl eventType
