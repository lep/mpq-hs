{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Codec.Archive.Mpq.Hashtable where

import Codec.Archive.Mpq.Crypto

import Data.Word

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Binary
import Data.Binary.Get
import Data.Bits

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8

import Control.Monad


data Hashtable = Hashtable
    { hashtableSize :: Word32
    , hashtableEntries :: Map Word32 HTEntry
    } deriving (Eq, Show)

data HTEntry = HTEntry
    { hashVal :: (Word32, Word32)
    , blocktableIndex :: Word32
    } deriving (Eq, Show)

lookup :: FilePath -> Hashtable -> Maybe HTEntry
lookup path ht = find False s
    where
        mask = hashtableSize ht -1
        k = hash path' TableOffset
        s = k .&. mask
        h = hashTuple path'

        path' = BS8.pack path

        find True ix | s == ix = Nothing
        find _ ix =
            case Map.lookup ix $ hashtableEntries ht of
                Nothing -> Nothing

                Just hte ->
                    if | blocktableIndex hte == 0xffffffff -> Nothing
                       | blocktableIndex hte == 0xfffffffe -> find True (succ ix .&. mask)
                       | hashVal hte == h -> Just hte
                       | otherwise -> find True (succ ix .&. mask)

pHashtable :: Int -> Get Hashtable
pHashtable numEntries = do
    allEntries <- Map.fromDistinctAscList . zip [0..] <$> replicateM numEntries pHTEntry
    let filtered = Map.filter (\hte -> blocktableIndex hte /= 0xffffffff) allEntries
    pure $ Hashtable (fromIntegral numEntries) filtered



decodeHashtable n bs = 
    let key = hash "(hash table)" FileKey
        bs' = decryptLazy key bs
    in runGet (pHashtable n) bs'

pHTEntry :: Get HTEntry
pHTEntry = HTEntry
    <$> ((,) <$> getWord32le <*> getWord32le)
    <*> (getWord32le *> getWord32le)
