{-# LANGUAGE OverloadedStrings #-}

-- | ASCII85 decode filter

module Pdf.Core.Stream.Filter.ASCII85Decode
(
  ascii85Decode
)
where

import Data.Word
import Data.Char (ord, isSpace)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Control.Exception hiding (throw)
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams

import Pdf.Core.Exception
import Pdf.Core.Object (Dict)
import Pdf.Core.Stream.Filter.Type

-- | ASCII85 decoder
ascii85Decode :: Maybe StreamFilter
ascii85Decode = Just StreamFilter
  { filterName = "ASCII85Decode"
  , filterDecode = decode
  }

decode :: Maybe Dict -> InputStream ByteString -> IO (InputStream ByteString)
decode _ is = do
  -- Read all input and decode
  chunks <- Streams.toList is
  let input = ByteString.concat chunks
  case decodeASCII85 input of
    Left err -> throwIO $ Corrupted ("ASCII85 decode error: " ++ err) []
    Right decoded -> Streams.fromByteString decoded

-- | Decode ASCII85 encoded data
decodeASCII85 :: ByteString -> Either String ByteString
decodeASCII85 input =
  let -- Remove whitespace and convert to list
      chars = filter (not . isSpace) $ map (toEnum . fromEnum) $ ByteString.unpack input
      -- Remove end marker if present
      dataChars = case dropWhile (/= '~') chars of
        [] -> chars  -- No end marker found
        _  -> takeWhile (/= '~') chars  -- Stop at end marker
      -- Process the characters
      decoded = decodeChars dataChars []
  in decoded

-- | Process ASCII85 characters
decodeChars :: String -> [Word8] -> Either String ByteString
decodeChars [] acc = Right $ ByteString.pack (reverse acc)
decodeChars ('z':rest) acc =
  -- 'z' represents four zero bytes
  decodeChars rest (0:0:0:0:acc)
decodeChars (c1:c2:c3:c4:c5:rest) acc = do
  -- Decode 5 ASCII85 characters to 4 bytes
  v1 <- charToValue c1
  v2 <- charToValue c2
  v3 <- charToValue c3
  v4 <- charToValue c4
  v5 <- charToValue c5
  let value = v1 * 85^(4::Int) + v2 * 85^(3::Int) + v3 * 85^(2::Int) + v4 * 85 + v5
      byte1 = fromIntegral ((value `div` 16777216) `mod` 256) :: Word8
      byte2 = fromIntegral ((value `div` 65536) `mod` 256) :: Word8
      byte3 = fromIntegral ((value `div` 256) `mod` 256) :: Word8
      byte4 = fromIntegral (value `mod` 256) :: Word8
  decodeChars rest (byte4:byte3:byte2:byte1:acc)
decodeChars [c1, c2] acc = do
  -- Partial group at end - 2 chars encode 1 byte
  -- Pad with 'u' (84) to make 5 chars, then take first byte
  v1 <- charToValue c1
  v2 <- charToValue c2
  let value = v1 * 85^(4::Int) + v2 * 85^(3::Int) + 84 * 85^(2::Int) + 84 * 85 + 84
      byte1 = fromIntegral ((value `div` 16777216) `mod` 256) :: Word8
  Right $ ByteString.pack (reverse (byte1:acc))
decodeChars [c1, c2, c3] acc = do
  -- Partial group at end - 3 chars encode 2 bytes
  -- Pad with 'u' (84) to make 5 chars, then take first 2 bytes
  v1 <- charToValue c1
  v2 <- charToValue c2
  v3 <- charToValue c3
  let value = v1 * 85^(4::Int) + v2 * 85^(3::Int) + v3 * 85^(2::Int) + 84 * 85 + 84
      byte1 = fromIntegral ((value `div` 16777216) `mod` 256) :: Word8
      byte2 = fromIntegral ((value `div` 65536) `mod` 256) :: Word8
  Right $ ByteString.pack (reverse (byte2:byte1:acc))
decodeChars [c1, c2, c3, c4] acc = do
  -- Partial group at end - 4 chars encode 3 bytes
  -- Pad with 'u' (84) to make 5 chars, then take first 3 bytes
  v1 <- charToValue c1
  v2 <- charToValue c2
  v3 <- charToValue c3
  v4 <- charToValue c4
  let value = v1 * 85^(4::Int) + v2 * 85^(3::Int) + v3 * 85^(2::Int) + v4 * 85 + 84
      byte1 = fromIntegral ((value `div` 16777216) `mod` 256) :: Word8
      byte2 = fromIntegral ((value `div` 65536) `mod` 256) :: Word8
      byte3 = fromIntegral ((value `div` 256) `mod` 256) :: Word8
  Right $ ByteString.pack (reverse (byte3:byte2:byte1:acc))
decodeChars [_] _ = Left "Invalid ASCII85 data: incomplete group"

-- | Convert ASCII85 character to value (0-84)
charToValue :: Char -> Either String Int
charToValue c
  | c >= '!' && c <= 'u' = Right (ord c - ord '!')
  | otherwise = Left $ "Invalid ASCII85 character: " ++ show c
