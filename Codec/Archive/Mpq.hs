{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Codec.Archive.Mpq where

import Codec.Archive.Mpq.Header
import Codec.Archive.Mpq.Crypto

import Codec.Archive.Mpq.Hashtable hiding (lookup)
import qualified Codec.Archive.Mpq.Hashtable as Hashtable

import Codec.Archive.Mpq.Blocktable hiding (lookup)
import qualified Codec.Archive.Mpq.Blocktable as Blocktable

import qualified Codec.Compression.Zlib as Zlib


import System.IO hiding (open)
import qualified System.IO as IO

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (liftIO, MonadIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import qualified Data.ByteString.Lazy as BL

import Data.ByteString.Builder

import Data.Binary
import Data.Bits

import Data.Binary.Get
import Data.Binary.Put

import System.FilePath (takeFileName)

import Prelude hiding (lookup)
import Data.List (genericLength)

import qualified Data.Sequence as Seq

import Data.IORef

newtype Archive = Archive { mpqRef :: IORef Mpq }

data Mpq = Mpq
    { header :: Header
    , hashtable :: Hashtable
    , blocktable :: Blocktable
    , fileHandle :: Handle
    , headerOffset :: Integer
    } deriving (Show)

writeMpqBlocktable mpq = do
    let gHeaderOffset = fromIntegral $ headerOffset mpq
        gBtPos = fromIntegral . btPos $ header mpq
    let x = gHeaderOffset + gBtPos
    hSeek (fileHandle mpq) AbsoluteSeek x
    let encoded = encodeBlocktable $ blocktable mpq
        decoded = decodeBlocktable (length . blocktableEntries $ blocktable mpq) encoded
    BS.hPut (fileHandle mpq) $ BL.toStrict encoded
    hFlush (fileHandle mpq)
    

updateBlocktableEntry mpq idx bte = 
    let bt' = Seq.adjust' (const bte) (fromIntegral idx) . blocktableEntries $ blocktable mpq
    in mpq { blocktable = Blocktable bt' }

headerByteSize = 32
hashtableByteSize n = fromIntegral n * 16
blocktableByteSize n = fromIntegral n * 16

close archive = do
    mpq <- readIORef $ mpqRef archive
    hFlush $ fileHandle mpq
    hClose $ fileHandle mpq
    writeIORef (mpqRef archive) $ error "Mpq closed"

open :: MonadIO m => FilePath -> m (Maybe Archive)
open path = runMaybeT $ do
    fh <- liftIO $ IO.openBinaryFile path ReadWriteMode
    headerPos <- findHeader fh
    h <- decode <$> liftIO (BL.hGet fh headerByteSize)

    let numHashtableEntries = fromIntegral $ htSize h
        hashtableBytes = hashtableByteSize numHashtableEntries
    liftIO $ hSeek fh AbsoluteSeek $ headerPos + fromIntegral (htPos h)
    ht <- liftIO $ decodeHashtable numHashtableEntries <$> BL.hGet fh hashtableBytes


    let numBlocktableEntries = fromIntegral $ btSize h
        blocktableBytes = blocktableByteSize numBlocktableEntries
    liftIO $ hSeek fh AbsoluteSeek $ headerPos + fromIntegral (btPos h)
    bt <- liftIO $ decodeBlocktable numBlocktableEntries <$> BL.hGet fh blocktableBytes

    let mpq = Mpq h ht bt fh headerPos

    Archive <$> liftIO (newIORef mpq)
    --pure $ Mpq h ht bt fh headerPos

findHeader :: MonadIO m => Handle -> MaybeT m Integer
findHeader fh = do
    liftIO $ hSeek fh AbsoluteSeek 0
    MaybeT $ find 0 =<< liftIO (hFileSize fh)

    where
        find p m | p >= m = return Nothing
        find p m = do
            header <- liftIO $ BS.hGet fh 4
            if header == "MPQ\x1a"
            then return $ Just p
            else do
                liftIO $ hSeek fh RelativeSeek (512-4)
                find (p+512) m

-- i've got no idea why i don't have this function available...
hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

--ceilDiv :: (Integral i, Integral j, Integral k) => i -> j -> k
ceilDiv a b = ceiling $ fromIntegral a / fromIntegral b

--sectorSize :: Mpq -> Int
sectorSize mpq = 512 * ( 1 `shiftL` s)
  where
    s = fromIntegral $ shiftSize $ header mpq

genericNormalSize = fromIntegral . normalSize
genericCompressedSize = fromIntegral . compressedSize

--readFile :: MonadIO m => Mpq -> FilePath -> m (Maybe HTEntry)
readFile archive fp = runMaybeT $ do
    mpq <- liftIO $ readIORef $ mpqRef archive
    htentry <- hoistMaybe $ Hashtable.lookup fp (hashtable mpq)
    btentry <- hoistMaybe $ Blocktable.lookup (blocktableIndex htentry) (blocktable mpq)
    extractFile mpq btentry fp
    --pure (htentry, btentry)

addFile archive fp bs =  do
    mpq <- liftIO $ readIORef $ mpqRef archive
    let htentry = Hashtable.lookup fp (hashtable mpq)
    mpq' <- case htentry of
        --Nothing -> addNewFile mpq fp bs
        Just hte -> replaceFile mpq fp bs hte
    writeIORef (mpqRef archive) mpq'


replaceFile mpq fp bs hte = do
    let Just bte = Blocktable.lookup (blocktableIndex hte) (blocktable mpq)
    liftIO $ hSeek (fileHandle mpq) SeekFromEnd 0
    offset <- liftIO $ fromIntegral <$> hFileSize (fileHandle mpq)
    let (compressedSize, compressed) = compressFile (sectorSize mpq) bs
    let bte = BTEntry
                { filePosition = (offset - fromIntegral (headerOffset mpq))
                , compressedSize = compressedSize
                , normalSize = (fromIntegral $ BL.length bs)
                , fileFlags = ( flagExists .|. flagCompressed)
                }
        mpq' = updateBlocktableEntry mpq (blocktableIndex hte) bte
    liftIO $ hPutBuilder (fileHandle mpq) compressed
    liftIO $ writeMpqBlocktable mpq'
    pure mpq'




compressFile chunkSize bs =
    let packed = map (pack chunkSize) $ chunksOf chunkSize bs
        chunkSizes = map (fromIntegral . BL.length) packed
        (sotSize, sotEncoded) = encodeSectorOffsetTable chunkSizes
    in (sotSize + sum chunkSizes, sotEncoded <> foldMap lazyByteString packed)

compress = BL.cons 0x02 . Zlib.compress
pack maxSize bs =
    let compressed = compress bs
    in if BL.length compressed < maxSize -2
    then compressed
    else bs

encodeSectorOffsetTable chunkSizes =
    let sotByteSize = 4 * (1 + genericLength chunkSizes)
        sot = scanl (+) sotByteSize chunkSizes
    in (sotByteSize, foldMap word32LE sot)


chunksOf s = go
  where
    go bs =
      case BL.splitAt s bs of
        (a, b)
            | BL.null a -> []
            | otherwise -> a:go b

extractFile :: MonadIO m => Mpq -> BTEntry -> FilePath -> m Builder
extractFile mpq bte fp = do
    liftIO $ hSeek (fileHandle mpq) AbsoluteSeek $ headerOffset mpq + fromIntegral (filePosition bte)
    if  | flagEnabled flagSingleUnit bte && not (flagEnabled flagCompressed bte) -> do
            liftIO $ lazyByteString . decrypt' baseKey1 <$> BL.hGet (fileHandle mpq) (generalSectorSize)

        | not ( flagEnabled flagCompressed bte) && not (flagEnabled flagSingleUnit bte) -> do
            let numSectors = ceilDiv (genericNormalSize bte) (sectorSize mpq :: Int) -1

            chunk1 <- liftIO $ foldMap lazyByteString <$> replicateM (numSectors - 1) (BL.hGet fh (sectorSize mpq))
            chunk2 <- liftIO $ lazyByteString <$> BL.hGet fh lastSectorSize
            pure $ chunk1 <> chunk2

        | flagEnabled flagSingleUnit bte && flagEnabled flagCompressed bte -> do
            liftIO $ lazyByteString . decompress . decrypt' baseKey1 <$> BL.hGet fh (genericCompressedSize bte)

        | otherwise -> do
            let numSectors = ceilDiv (genericNormalSize bte) (sectorSize mpq :: Int)
            sectorOffsetTable <- liftIO $ parseSectorOffsetTable (decrypt' $ baseKey1 -1) (succ numSectors) <$> BL.hGet fh (succ numSectors * 4)
            go (zipWith (-) (tail sectorOffsetTable) sectorOffsetTable) 0 (mempty :: Builder)
            

    where
        lastSectorSize = genericNormalSize bte `mod` sectorSize mpq
        generalSectorSize = genericNormalSize bte

        go [size] idx output = do
            let thisSectorSize 
                    | lastSectorSize == 0 = generalSectorSize
                    | otherwise = size
            chunk <- liftIO $ decrypt' (baseKey1 + idx) <$> BL.hGet fh thisSectorSize
            if thisSectorSize == generalSectorSize
            then let c = lazyByteString chunk
                 in pure (output <> c)
            else let c = lazyByteString $ decompress chunk
                 in pure (output <> c)


        go (size:ss) idx output = do
            chunk <- liftIO $ decrypt' (baseKey1 + idx) <$> BL.hGet fh size
            if size == generalSectorSize
            then let c = lazyByteString chunk
                 in go ss (succ idx) (output <> c)
            else let c = lazyByteString $ decompress chunk
                 in go ss (succ idx) (output <> c)

        fh = fileHandle mpq
        decrypt'
            | flagEnabled flagEncrypted bte = decryptLazy
            | otherwise = \_ bs -> bs

        baseKey0 = hash (BS8.pack $ takeFileName fp) FileKey
        baseKey1
            | flagEnabled flagAdjustedKey bte = (baseKey0 + filePosition bte) `xor` normalSize bte
            | otherwise = baseKey0

parseSectorOffsetTable decrypt numEntries bs =
    let bs' = decrypt bs
    in runGet (replicateM numEntries (fromIntegral <$> getWord32le)) bs'



decompress bs =
    case BL.uncons bs of
        Just (0x02, bs') -> Zlib.decompress bs'
        _ -> error "Only ZLIB compression supported"
