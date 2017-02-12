module Main where

import Foreign.C

foreign export ccall square :: CInt -> CInt

square :: CInt -> CInt
square x = x * x

foreign import ccall "runNSApplication" runNSApplication :: IO ()

main :: IO ()
main = do
  putStrLn "hello world"
  runNSApplication
