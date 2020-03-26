{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications         #-}

module Main where

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
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString


fitch :: (Bits b) => b -> b -> (b, Word)
fitch l r
  | popCount (l .&. r) > 0 = (l .&. r, 0)
  | otherwise              = (l .|. r, 1)

fitch' :: (Bits b) => b -> b -> b
fitch' l r
  | popCount (l .&. r) > 0 = l .&. r
  | otherwise              = l .|. r


fitchByteString' :: ByteString -> ByteString -> ByteString
fitchByteString' l r =
  ByteString.pack (ByteString.zipWith (fitch') l r)

fitchUnbox'
  :: Unboxed.Vector Word8 -> Unboxed.Vector Word8 -> Unboxed.Vector Word8
fitchUnbox' = Unboxed.zipWith fitch'


fitchUnboxedSequence :: (Bits a, Unboxed.Unbox a)
  =>  Unboxed.Vector a -> Unboxed.Vector a -> Unboxed.Vector (a, Word)
fitchUnboxedSequence =
  Unboxed.zipWith fitch

fitchStorableSequence :: (Bits a, Storable.Storable a)
  =>  Storable.Vector a -> Storable.Vector a -> Storable.Vector a
fitchStorableSequence =
  Storable.zipWith fitch'

fitchBoxedSequence :: (Bits a)
  => Boxed.Vector a -> Boxed.Vector a -> Boxed.Vector (a, Word)
fitchBoxedSequence =
  Boxed.zipWith fitch


len :: Int
len = 100000

unboxInput :: Unboxed.Vector Word8
unboxInput = Unboxed.fromList $ take len (cycle [1,2,4,3])

storeInput :: Storable.Vector Word8
storeInput = Storable.fromList $ take len (cycle [1,2,4,3])


boxInput :: Boxed.Vector Natural
boxInput = Boxed.fromList $ take len (cycle [1,2,4,3])

unboxIntInput :: Unboxed.Vector Word64
unboxIntInput = Unboxed.fromList $ take len (cycle [1,2,4,3])

byteStringInput :: ByteString
byteStringInput = ByteString.pack $ take len (cycle [1..4])

main :: IO ()
main = C.defaultMain
     [ C.bgroup "characters" 
       [ C.bench "fitch-unboxed-Word8"    $ C.nf (fitchUnboxedSequence  unboxInput) unboxInput
--       , C.bench "fitch"   $ C.nf (fitchStorableSequence storeInput) storeInput
       , C.bench "fitch-unboxed-Word64" $ C.nf (fitchUnboxedSequence  unboxIntInput) unboxIntInput
       , C.bench "fitch-boxed-natural"      $ C.nf (fitchBoxedSequence boxInput) boxInput
       , C.bench "fitch'-unbox" $ C.nf (fitchUnbox' unboxInput) unboxInput
       , C.bench "fitch'-bytestring" $ C.nf (fitchByteString' byteStringInput) byteStringInput
       ]
     ]
