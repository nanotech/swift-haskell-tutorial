module Main where

import Control.Concurrent (forkIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Text as T
import Data.Word (Word8)

import Foreign.C (CChar (CChar), CInt (CInt), CSize (CSize))
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import Foreign.Marshal.Array (copyArray, mallocArray)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr)
import Foreign.Storable (poke)

foreign export ccall square :: CInt -> CInt

square :: CInt -> CInt
square x = x * x

foreign import ccall "runNSApplication" runNSApplication :: IO ()

main :: IO ()
main = do
  putStrLn "hello world"
  runNSApplication


-- Examples

-- Swift to Haskell ByteString Example

foreign export ccall countBytes :: Word8 -> Ptr CChar -> CSize -> IO CSize

countBytes :: Word8 -> Ptr CChar -> CSize -> IO CSize
countBytes needle haystack haystackLen = do
  s <- B.packCStringLen (haystack, fromIntegral haystackLen)
  pure (B.foldl (\count b -> count + if b == needle then 1 else 0) 0 s)

-- Haskell to Swift ByteString Example

mallocCopyByteString :: ByteString -> IO (Ptr CChar, Int)
mallocCopyByteString s =
  BU.unsafeUseAsCStringLen s $ \(p, n) -> do
    a <- mallocArray n
    copyArray a p n
    pure (a, n)

foreign export ccall getSequence :: Ptr CSize -> IO (Ptr CChar)

getSequence :: Ptr CSize -> IO (Ptr CChar)
getSequence sizePtr = do
  (p, n) <- mallocCopyByteString (B.pack [1..10])
  poke sizePtr (fromIntegral n)
  pure p

-- Swift to Haskell Callback Example

foreign export ccall callbackExample :: FunPtr (CInt -> IO ()) -> IO ()
foreign import ccall "dynamic" unwrapCallback :: FunPtr (CInt -> IO ()) -> (CInt -> IO ())

callbackExample :: FunPtr (CInt -> IO ()) -> IO ()
callbackExample f = (unwrapCallback f) 3

-- Swift to Haskell Callback with Context Example

foreign export ccall contextCallbackExample
    :: Ptr ()
    -> FunPtr (Ptr () -> IO ())
    -> FunPtr (Ptr () -> CInt -> IO ())
    -> IO ()
foreign import ccall "dynamic" unwrapContextCallback
    :: FunPtr (Ptr () -> CInt -> IO ())
    -> (Ptr () -> CInt -> IO ())

contextCallbackExample
    :: Ptr ()                           -- ^ Context pointer
    -> FunPtr (Ptr () -> IO ())         -- ^ Context release function
    -> FunPtr (Ptr () -> CInt -> IO ()) -- ^ Callback function
    -> IO ()
contextCallbackExample ctxp releaseCtx callbackPtr = do
  ctxfp <- newForeignPtr releaseCtx ctxp
  let callback :: CInt -> IO ()
      callback result = withForeignPtr ctxfp $ \ctxp' ->
        (unwrapContextCallback callbackPtr) ctxp' result
  _ <- forkIO $ do
      let result = 3 -- perform your complex computation here
      callback result
  pure ()

-- Swift to Haskell Function Example

foreign export ccall makeMultiplier :: CInt -> IO (FunPtr (CInt -> CInt))
foreign import ccall "wrapper" wrapMultiplier
    :: (CInt -> CInt)
    -> IO (FunPtr (CInt -> CInt))

makeMultiplier :: CInt -> IO (FunPtr (CInt -> CInt))
makeMultiplier x = wrapMultiplier (x *)

foreign export ccall freeMultiplier :: FunPtr (CInt -> CInt) -> IO ()

freeMultiplier :: FunPtr (CInt -> CInt) -> IO ()
freeMultiplier = freeHaskellFunPtr
