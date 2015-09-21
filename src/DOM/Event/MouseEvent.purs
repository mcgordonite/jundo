module DOM.Event.MouseEvent where

import Prelude
import DOM.Event.Types
import Unsafe.Coerce

foreign import data MouseEvent :: *

data MouseEventType = MouseEventType EventType
click

toEvent :: MouseEvent -> Event
toEvent = unsafeCoerce

foreign import movementX :: MouseEvent -> Number
foreign import movementY :: MouseEvent -> Number

foreign import mouseEventListener :: forall eff a. (MouseEvent -> Eff (eff) a) -> EventListener eff

addMouseEventListener :: MouseEventType