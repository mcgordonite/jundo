module Monad where

import Prelude
import Control.Monad.Reader

data Context = Context Int

instance showContext :: Show Context where
	show (Context id) = "Context " ++ show id

data Buffer = Buffer Int Int Int Int

instance showBuffer :: Show Buffer where
	show (Buffer id x y z) = "Buffer " ++ show id ++ " [" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ "]"

type WebGL a = Reader Context a

runWebGL :: forall a. Context -> WebGL a -> a
runWebGL ctx x = runReader x ctx

createBuffer :: WebGL Buffer
createBuffer = do
	Context id <- ask
	return (Buffer id 0 0 0)

initContext :: String -> Context
initContext _ = Context 0

testWebGL :: String
testWebGL = runWebGL (initContext "easel") do
	buffer <- createBuffer
	return $ show buffer
