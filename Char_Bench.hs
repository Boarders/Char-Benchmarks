{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector         as Boxed
import qualified Criterion.Main        as C (bench, bgroup, defaultMain, nf)
import Data.Bits
import GHC.Natural
import GHC.Word
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Ptr


foreign import ccall
  "mmfitch" mmfitch :: CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong

fitch128FFI :: Storable.Vector Word64 -> Storable.Vector Word64 -> Storable.Vector Word64
fitch128FFI = undefined


fitch128Unbox
  :: Unboxed.Vector (Word64, Word64)
  -> Unbxoed.Vector (Word64, Word64)
  -> Unboxed.Vector (Word64, Word64)
fitch128Unbox = undefined



fitch :: (Bits b) => b -> b -> (b, Word)
fitch l r
  | popCount (l .&. r) > 0 = (l .&. r, 0)
  | otherwise              = (l .|. r, 1)

fitchUnboxed :: (Bits b, Unboxed.Unbox b) => b -> b -> (b, Word)
fitchUnboxed l r
  | popCount (l .&. r) > 0 = (l .&. r, 0)
  | otherwise              = (l .|. r, 1)

fitchStorable :: (Bits b, Storable.Storable b) => b -> b -> b
fitchStorable l r
  | popCount (l .&. r) > 0 = l .&. r
  | otherwise              = l .|. r

fitchUnboxedSequence :: (Bits a, Unboxed.Unbox a)
  =>  Unboxed.Vector a -> Unboxed.Vector a -> Unboxed.Vector (a, Word)
fitchUnboxedSequence =
  Unboxed.zipWith fitchUnboxed

fitchStorableSequence :: (Bits a, Storable.Storable a)
  =>  Storable.Vector a -> Storable.Vector a -> Storable.Vector a
fitchStorableSequence =
  Storable.zipWith fitchStorable

fitchBoxedSequence :: (Bits a)
  => Boxed.Vector a -> Boxed.Vector a -> Boxed.Vector (a, Word)
fitchBoxedSequence =
  Boxed.zipWith fitch


len :: Int
len = 100

unboxInput :: Unboxed.Vector Word8
unboxInput = Unboxed.fromList $ take len (cycle [1,2,4,3])

storeInput :: Storable.Vector Word8
storeInput = Storable.fromList $ take len (cycle [1,2,4,3])


boxInput :: Boxed.Vector Natural
boxInput = Boxed.fromList $ take len (cycle [1,2,4,3])


main :: IO ()
main = C.defaultMain
     [ C.bgroup "characters" 
       [ C.bench "fitchUnboxed"  $ C.nf (fitchUnboxedSequence  unboxInput) unboxInput
       , C.bench "fitchStorable" $ C.nf (fitchStorableSequence storeInput) storeInput
       , C.bench "fitchBoxed"    $ C.nf (fitchBoxedSequence boxInput) boxInput
       ]
     ]
