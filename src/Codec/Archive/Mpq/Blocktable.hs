{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Codec.Archive.Mpq.Blocktable where

import Codec.Archive.Mpq.Crypto
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy as Lazy

newtype Blocktable = Blocktable
  { blocktableEntries :: Seq BTEntry
  }
  deriving (Show, Eq)

data BTEntry = BTEntry
  { filePosition :: Word32,
    compressedSize :: Word32,
    normalSize :: Word32,
    fileFlags :: Word32
  }
  deriving (Show, Eq)

flagImploded, flagCompressed, flagEncrypted :: Word32
flagAdjustedKey, flagSingleUnit, flagExists :: Word32
flagImploded = 0x00000100
flagCompressed = 0x00000200
flagEncrypted = 0x00010000

flagAdjustedKey = 0x00020000

flagSingleUnit = 0x01000000

flagExists = 0x80000000

flagEnabled :: Word32 -> BTEntry -> Bool
flagEnabled mask bte = mask .&. fileFlags bte /= 0

data FileFlags
  = Compressed
  | Imploded
  | Encrypted
  | AjustedKey
  | SingleUnit
  | Exists
  deriving (Eq, Ord, Show)

eBlocktable :: Blocktable -> PutM ()
eBlocktable bt = do
  forM_ (blocktableEntries bt) $ \bte ->
    eBTEntry bte

eBTEntry :: BTEntry -> PutM ()
eBTEntry (BTEntry a b c d) = do
  putWord32le a
  putWord32le b
  putWord32le c
  putWord32le d

pBlocktable :: Int -> Get Blocktable
pBlocktable numEntries =
  Blocktable . Seq.fromList <$> replicateM numEntries pBTEntry

pBTEntry :: Get BTEntry
pBTEntry =
  BTEntry
    <$> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le

decodeBlocktable :: Int -> Lazy.ByteString -> Blocktable
decodeBlocktable n bs =
  let key = hash "(block table)" FileKey
      bs' = decryptLazy key bs
   in runGet (pBlocktable n) bs' -- bs'

encodeBlocktable :: Blocktable -> Lazy.ByteString
encodeBlocktable bt =
  let key = hash "(block table)" FileKey
      bs = runPut $ eBlocktable bt
   in encryptLazy key bs

lookup :: (Integral i) => i -> Blocktable -> Maybe BTEntry
lookup i = Seq.lookup (fromIntegral i) . blocktableEntries

ceilDiv a b = ceiling $ fromIntegral a / fromIntegral b
