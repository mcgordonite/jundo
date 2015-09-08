module Main where

import Prelude
import Graphics.WebGL.Free
import Control.Monad.Eff
import Control.Monad.Eff.Console

main :: Eff (console :: CONSOLE) Unit
main = do
	log "Done!"