module Codec.Archive.Mpq.Crypto where

-- TODO: redo/clean up

import Data.Array
import Data.Word
import Data.Bits
import Data.Char
import Data.List

import Control.Arrow
import Control.Monad.State

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Char8 as Strict8

import Data.Binary.Get
import Data.Binary.Put

cryptTable :: Array Word32 Word32
cryptTable = array (0, 0x500) . concat . flip evalState 0x00100001 $
    forM [0..0x100] $ \index1 ->
        forM (take 5 [index1, index1+0x100..]) $ \index2 -> do
            seed <- get
            let seed1 = (seed * 125 +3) `mod` 0x2AAAAB
                temp1 = (seed1 .&. 0xFFFF) `shiftL` 0x10
                seed2 = (seed1 * 125 +3) `mod` 0x2AAAAB
                temp2 = seed2 .&. 0xFFFF
            put seed2
            return (index2, temp1 .|. temp2)

data HashType = TableOffset | NameA | NameB | FileKey
    deriving (Show, Enum)

--hash :: String -> HashType -> Word32
hash :: Strict.ByteString -> HashType -> Word32
hash fileName hashType = fst $ Strict8.foldl loop (0x7FED7FED, 0xEEEEEEEE) fileName
    where
        hashTypeInt = fromIntegral . fromEnum $ hashType
        loop (seed1, seed2) currentChar =
            let ch = fromIntegral . ord . toUpper $ currentChar
                seed1' = (cryptTable ! (hashTypeInt `shiftL` 8 + ch)) `xor` (seed1 + seed2)
                seed2' = ch + seed1' + seed2 + (seed2 `shiftL` 5) + 3
            in (seed1', seed2')

decryptBlock :: [Word32] -> Word32 -> [Word32]
decryptBlock block key = snd $ mapAccumL loop (key, 0xEEEEEEEE) block
    where loop (key, seed) cur =
            let seed' = seed + (cryptTable ! (0x400 + key .&. 0xff))
                c = cur `xor` (key + seed')
                key' = ((complement key `shiftL` 0x15) + 0x11111111) .|. (key `shiftR` 0x0b)
                seed'' = c + seed' + (seed' `shiftL` 5) + 3
            in ((key', seed''),  c)

encryptBlock :: [Word32] -> Word32 -> [Word32]
encryptBlock block key = snd $ mapAccumL loop (key, 0xEEEEEEEE) block
    where loop (key, seed) cur =
            let seed' = seed + (cryptTable ! (0x400 + (key .&. 0xff)))
                c = cur `xor` (key + seed')
                key' = ((complement key `shiftL` 0x15) + 0x11111111) .|. (key `shiftR` 0x0b)
                seed'' = cur + seed' + (seed' `shiftL` 5) + 3
            in ((key', seed''), c)




toWord32 :: Lazy.ByteString -> [Word32]
toWord32 s = case runGetOrFail loop s of
                Left x -> error $ unwords ["toWord32:", show x]
                Right (_, _, r) -> r
    where
        loop = do
            empty <- isEmpty
            if empty
                then return []
                else do
                    w <- getWord32le
                    rest <- loop
                    return (w : rest)

fromWord32 :: [Word32] -> Lazy.ByteString
fromWord32 = runPut . mapM_ putWord32le

decrypt:: Word32 -> Strict.ByteString -> Strict.ByteString
--decrypt key = Lazy.toStrict
--                . fromWord32 . (`decryptBlock` key) . toWord32
--            . Lazy.fromStrict
decrypt key str =
    let rounded = 4*(Strict.length str `div` 4)
        (a, b) = Strict.splitAt rounded str
        a' = Lazy.toStrict . fromWord32 . (`decryptBlock` key) . toWord32 $ Lazy.fromStrict a
    in a' `Strict.append` b

decryptLazy :: Word32 -> Lazy.ByteString -> Lazy.ByteString
decryptLazy key str =
    let rounded = 4 * Lazy.length str `div` 4
        (a, b) = Lazy.splitAt rounded str
        a' = fromWord32 . (`decryptBlock` key) $ toWord32 a
    in a' <> b

encryptLazy :: Word32 -> Lazy.ByteString -> Lazy.ByteString
encryptLazy key str =
    let rounded = 4 * Lazy.length str `div` 4
        (a, b) = Lazy.splitAt rounded str
        a' = fromWord32 . (`encryptBlock` key) $ toWord32 a
    in a' <> b

hashTuple :: Strict.ByteString -> (Word32, Word32)
hashTuple = (`hash` NameA) &&& (`hash` NameB)
