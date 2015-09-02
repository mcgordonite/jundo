module Main where

import Prelude
import WebGL
import Control.Monad.Eff
import Control.Monad.Eff.Console

main :: Eff (console :: CONSOLE) Unit
main = do
	logBuffer $ runWebGL (unsafeGetContext "easel") do
		b1 <- createBuffer
		b2 <- createBuffer
		return b2
	log "Done!"
