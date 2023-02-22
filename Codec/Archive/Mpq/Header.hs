{-# LANGUAGE DeriveGeneric #-}
module Codec.Archive.Mpq.Header where

import Data.Word

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Binary
import Data.Binary.Get

import Data.ByteString (ByteString)

import Control.Monad

import GHC.Generics

data Header = Header
    { headerSize :: Word32
    , archiveSize :: Word32
    , version :: Word16
    , shiftSize :: Word16
    , htPos :: Word32
    , btPos :: Word32
    , htSize :: Word32
    , btSize :: Word32
    } deriving (Show, Generic)


instance Binary Header where
    get =  Header
        <$> getWord32le
        <*> getWord32le
        <*> getWord16le
        <*> getWord16le
        <*> getWord32le
        <*> getWord32le
        <*> getWord32le
        <*> getWord32le

