{-# LANGUAGE OverloadedStrings #-}

-- | ASCII85 decode filter

module Pdf.Core.Stream.Filter.ASCII85Decode
(
  ascii85Decode
)
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char (isSpace)
import qualified Codec.Binary.Base85 as Base85
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
      cleaned = preprocessASCII85 input
  case Base85.decode cleaned of
    Left (remaining, offset) ->
      throwIO $ Corrupted ("ASCII85 decode error at offset: " ++ show offset ++ ", remaining: " ++ show remaining) []
    Right decoded -> Streams.fromByteString decoded

-- | Preprocess ASCII85 data: remove whitespace and strip end marker
preprocessASCII85 :: ByteString -> ByteString
preprocessASCII85 input =
  let -- Remove whitespace
      noWhitespace = ByteString.filter (not . isSpace . toEnum . fromEnum) input
      -- Remove end marker (~>) if present
      stripped = case ByteString.breakSubstring "~>" noWhitespace of
        (before, after)
          | ByteString.null after -> noWhitespace
          | otherwise -> before
  in stripped
