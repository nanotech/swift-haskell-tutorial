module Main (main) where

import Foreign
import Foreign.C

import Lib

foreign import ccall "swiftAppMain" swiftAppMain :: FunPtr (CInt -> CInt) -> IO ()
foreign import ccall "wrapper" wrapSquare :: (CInt -> CInt) -> IO (FunPtr (CInt -> CInt))

main :: IO ()
main = do
  putStrLn "Hello"
  cSquare <- wrapSquare square
  swiftAppMain cSquare
