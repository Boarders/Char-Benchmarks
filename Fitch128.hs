{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications         #-}

module Fitch128 where

import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Storable as Storable
import Data.Vector.Storable.Mutable (new)
import qualified Data.Vector         as Boxed
import qualified Criterion.Main        as C (bench, bgroup, defaultMain, nf)
import Data.Bits
import GHC.Natural
import GHC.Word
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
import Data.Coerce
import Unsafe.Coerce
import Data.Int


foreign import ccall
  "mmfitch" mmfitch :: CLong -> Ptr CULong -> Ptr CULong -> Ptr CULong

fitch128FFI :: Storable.Vector Word64 -> Storable.Vector Word64 -> Storable.Vector Word64
fitch128FFI vec1 vec2 =
  let
    len :: Int
    len = Storable.length $ vec1
    len' :: CLong
    len'  = fromIntegral len
    v1, v2 :: Storable.Vector CULong
    v1 = unsafeCoerce v1
    v2 = unsafeCoerce v2
    v3 :: Storable.Vector CULong
    v3 = Storable.create $ new len
  in
    unsafePerformIO $
      Storable.unsafeWith v1 $ \ptr1 ->
      Storable.unsafeWith v2 $ \ptr2 ->
        do
          let resPtr = castPtr @_ @Word64 $ mmfitch len' ptr1 ptr2
          fPtrRes <- newForeignPtr_ resPtr
          pure . (flip Storable.unsafeFromForeignPtr0) len $ fPtrRes


fitchFFIInput :: Storable.Vector Word64
fitchFFIInput = undefined
--  unsafePerformIO $
--    Storable.unsafeFromForeignPtr0 
                                  
       
  


type Word128 = (Word64, Word64)

fitch128Unbox
  :: Unboxed.Vector Word128
  -> Unboxed.Vector Word128
  -> Unboxed.Vector (Word128, Word)
fitch128Unbox =
  Unboxed.zipWith fitch128


fitch128 :: Word128 -> Word128 -> (Word128, Word)
fitch128 l r
  | popCount128 (l `and128` r) > 0 = (l `and128` r, 0)
  | otherwise                      = (l `or128`  r, 1)

popCount128 :: Word128 -> Int
popCount128 (w1, w2) = popCount w1 + popCount w2


and128 :: Word128 -> Word128 -> Word128
and128 (w1, w2) (w1', w2') = (w1 .&. w1', w2 .&. w2')

or128 :: Word128 -> Word128 -> Word128
or128 (w1, w2) (w1', w2') = (w1 .|. w1', w2 .|. w2')
