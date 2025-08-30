{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Codec.Archive.Mpq.Blocktable where

import Codec.Archive.Mpq.Crypto

import Data.Word

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Data.ByteString (ByteString)

import Control.Monad


import Data.Bits


newtype Blocktable = Blocktable
    { blocktableEntries :: Seq BTEntry
    } deriving (Show, Eq)

data BTEntry = BTEntry
    { filePosition :: Word32
    , compressedSize :: Word32
    , normalSize :: Word32
    , fileFlags :: Word32
    } deriving (Show, Eq)



flagImploded, flagCompressed, flagEncrypted :: Word32
flagAdjustedKey, flagSingleUnit, flagExists ::  Word32
flagImploded    = 0x00000100
flagCompressed  = 0x00000200
flagEncrypted   = 0x00010000
flagAdjustedKey = 0x00020000
flagSingleUnit  = 0x01000000
flagExists      = 0x80000000

flagEnabled :: Word32 -> BTEntry -> Bool
flagEnabled mask bte = mask .&. fileFlags bte /= 0

data FileFlags =
      Compressed
    | Imploded
    | Encrypted
    | AjustedKey
    | SingleUnit
    | Exists
    deriving (Eq, Ord, Show)

eBlocktable bt = do
    forM_ (blocktableEntries bt) $ \bte ->
        eBTEntry bte

eBTEntry (BTEntry a b c d) = do
    putWord32le a
    putWord32le b
    putWord32le c
    putWord32le d

pBlocktable :: Int -> Get Blocktable
pBlocktable numEntries =
    Blocktable . Seq.fromList <$> replicateM numEntries pBTEntry


pBTEntry =
    BTEntry <$> getWord32le
            <*> getWord32le
            <*> getWord32le
            <*> getWord32le

decodeBlocktable n bs = 
    let key = hash "(block table)" FileKey
        bs' = decryptLazy key bs
    in runGet (pBlocktable n) bs' -- bs'

encodeBlocktable bt =
    let key = hash "(block table)" FileKey
        bs = runPut $ eBlocktable bt
    in encryptLazy key bs


lookup :: Integral i => i -> Blocktable -> Maybe BTEntry
lookup i = Seq.lookup (fromIntegral i) . blocktableEntries
