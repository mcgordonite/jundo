module DOM.Event.KeyboardEvent (
	KeyboardEventType(),
	keydown,
	keypress,
	keyup,
	keyCode,
	wKeyCode,
	aKeyCode,
	sKeyCode,
	dKeyCode,
	addKeyboardEventListener
	) where

import Prelude
import Control.Monad.Eff
import DOM
import DOM.Event.Types
import qualified DOM.Event.EventTypes as D

data KeyboardEventType = KeyboardEventType EventType
keydown = KeyboardEventType D.keydown
keypress = KeyboardEventType D.keypress
keyup = KeyboardEventType D.keyup

foreign import keyCode :: KeyboardEvent -> Int

wKeyCode :: Int
wKeyCode = 87

aKeyCode :: Int
aKeyCode = 65

sKeyCode :: Int
sKeyCode = 83

dKeyCode :: Int
dKeyCode = 68

foreign import data KeyboardEventListener :: # ! -> *
foreign import keyboardEventListener :: forall eff a. (KeyboardEvent -> Eff eff a) -> KeyboardEventListener eff
foreign import addKeyboardEventListenerImpl :: forall eff. EventType -> KeyboardEventListener (dom :: DOM | eff) -> Boolean -> EventTarget -> Eff (dom :: DOM | eff) Unit

addKeyboardEventListener :: forall eff a. KeyboardEventType -> (KeyboardEvent -> Eff (dom :: DOM | eff) a) -> Boolean -> EventTarget -> Eff (dom :: DOM | eff) Unit
addKeyboardEventListener (KeyboardEventType event) callback useCapture target =
	addKeyboardEventListenerImpl event (keyboardEventListener callback) useCapture target
