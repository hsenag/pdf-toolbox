{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Cross reference

module Pdf.Core.XRef
(
  XRef(..),
  Entry(..),
  readXRef,
  lastXRef,
  prevXRef,
  trailer,
  lookupTableEntry,
  lookupStreamEntry,
  isTable,
  UnknownXRefStreamEntryType(..),
)
where

import Pdf.Core.Object
import Pdf.Core.Object.Util
import Pdf.Core.Parsers.XRef
import Pdf.Core.Stream
import Pdf.Core.Exception
import Pdf.Core.Util
import Pdf.Core.IO.Buffer (Buffer)
import qualified Pdf.Core.IO.Buffer as Buffer

import Data.Typeable
import Data.Int
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import Control.Exception hiding (throw)
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

-- | Entry in cross reference stream
data Entry =
  -- | Object number and generation
  EntryFree Int Int |
  -- | Object offset (in bytes from the beginning of file) and generation
  EntryUsed Int64 Int |
  -- | Object number of object stream and index within the object stream
  EntryCompressed Int Int
  deriving (Eq, Show)

-- | Cross reference
data XRef =
  -- | Offset
  XRefTable Int64 |
  -- | Offset and stream
  XRefStream Int64 Stream
  deriving (Eq, Show)

-- | Check whether the stream starts with \"xref\" keyword.
-- The keyword itself and newline after it are consumed
isTable :: InputStream ByteString -> IO Bool
isTable is = (Streams.parseFromStream tableXRef is >> return True)
  `catch` \(Streams.ParseException _) -> return False

-- | Find the last cross reference
lastXRef :: Buffer -> IO XRef
lastXRef buf = do
  sz <- Buffer.size buf
  Buffer.seek buf $ max 0 (sz - 1024)
  off <- Streams.parseFromStream startXRef (Buffer.toInputStream buf)
    `catch` \(Streams.ParseException msg) ->
                  throwIO (Corrupted "lastXRef" [msg])
  readXRef buf off `catches`
    [ Handler $ \e@Corrupted{} -> recover off e
    , Handler $ \(Streams.ParseException msg) ->
                  recover off (Corrupted "lastXRef" [msg])
    ]
  where
  -- Some producers write a startxref offset that doesn't point at the xref.
  -- SAP's RSTXPDF, for example, gets it wrong by the length of a comment it
  -- adds to the header, even though the offsets within the table are right.
  -- Fall back to searching the file for the xref keyword.
  recover off e = do
    found <- searchXRefTable buf
    case found of
      Just off' | off' /= off -> readXRef buf off'
      _ -> throwIO e

-- | Offset of the last @xref@ keyword that starts a line, if there is one
searchXRefTable :: Buffer -> IO (Maybe Int64)
searchXRefTable buf =
  -- only recognise the keyword at the start of a line, which also stops the
  -- "xref" inside "startxref" from matching
  fmap (fmap (+ 1)) (lastOccurrence buf ["\nxref", "\rxref"])

-- | Offset of the last occurrence of any of the needles in the whole file
lastOccurrence :: Buffer -> [ByteString] -> IO (Maybe Int64)
lastOccurrence buf needles = do
  Buffer.seek buf 0
  go 0 ByteString.empty Nothing
  where
  -- carrying this many bytes over to the next chunk is enough for a match
  -- that straddles the boundary to still be found
  overlap = maximum (1 : map ByteString.length needles) - 1

  go base prefix found = do
    chunk <- Buffer.read buf
    case chunk of
      Nothing -> return found
      Just bs -> do
        let haystack = prefix `ByteString.append` bs
            hits = concatMap (`substringOffsets` haystack) needles
            -- any hit here is later in the file than anything found so far
            found' =
              case hits of
                [] -> found
                _ -> Just (base + fromIntegral (maximum hits))
            keep = min (ByteString.length haystack) overlap
            consumed = ByteString.length haystack - keep
        go (base + fromIntegral consumed)
           (ByteString.drop consumed haystack)
           found'

-- | Offsets of every occurrence of the first bytestring in the second
substringOffsets :: ByteString -> ByteString -> [Int]
substringOffsets needle = go 0
  where
  go base hay =
    let (before, rest) = ByteString.breakSubstring needle hay
    in if ByteString.null rest
         then []
         else let off = base + ByteString.length before
              in off : go (off + 1) (ByteString.drop 1 rest)

-- | Read XRef at specified offset
readXRef :: Buffer -> Int64 -> IO XRef
readXRef buf off = do
  Buffer.seek buf off
  let is = Buffer.toInputStream buf
  table <- isTable is
  if table
    then return (XRefTable off)
    else do
      s <- readStream is off
      return (XRefStream off s)

-- | Find prev cross reference
prevXRef :: Buffer -> XRef -> IO (Maybe XRef)
prevXRef buf xref = message "prevXRef" $ do
  tr <- trailer buf xref
  case HashMap.lookup "Prev" tr of
    Just prev -> do
      off <- sure $ intValue prev
        `notice` "Prev in trailer should be an integer"
      Just <$> readXRef buf (fromIntegral off)
    _ -> return Nothing

-- | Read trailer for the xref
trailer :: Buffer -> XRef -> IO Dict
trailer buf (XRefTable off) = do
  Buffer.seek buf off
  let is = Buffer.toInputStream buf
  table <- isTable is
  unless table $
    throwIO (Unexpected "trailer" ["table not found"])
  ( skipTable is >>
    Streams.parseFromStream parseTrailerAfterTable is
    ) `catch` \(Streams.ParseException msg) ->
                  throwIO (Corrupted "trailer" [msg])
trailer _ (XRefStream _ (S dict _)) = return dict

skipTable :: InputStream ByteString -> IO ()
skipTable is = message "skipTable" $
  (subsectionHeader is
    `catch` \(Streams.ParseException msg) ->
      throwIO (Corrupted msg []))
    >>= go . snd
  where
  go count = nextSubsectionHeader is count >>= maybe (return ()) (go . snd)

subsectionHeader :: InputStream ByteString -> IO (Int, Int)
subsectionHeader = Streams.parseFromStream parseSubsectionHeader

nextSubsectionHeader :: InputStream ByteString -> Int -> IO (Maybe (Int, Int))
nextSubsectionHeader is count = message "nextSubsectionHeader" $ do
  skipSubsection is count
  fmap Just (subsectionHeader is)
    `catch` \(Streams.ParseException _) -> return Nothing

skipSubsection :: InputStream ByteString -> Int -> IO ()
skipSubsection is count = Buffer.dropExactly (count * 20) is

-- | Read xref entry for the indirect object from xref table
lookupTableEntry :: Buffer
                 -> XRef  -- ^ should be xref table
                 -> Ref   -- ^ indirect object to look for
                 -> IO (Maybe Entry)
lookupTableEntry buf (XRefTable tableOff) (R index gen)
  = message "lookupTableEntry" $ do
  Buffer.seek buf tableOff
  table <- isTable (Buffer.toInputStream buf)
  unless table $
    throwIO $ Unexpected "Not a table" []
  (subsectionHeader (Buffer.toInputStream buf) >>= go)
    `catch` \(Streams.ParseException err) -> throwIO (Corrupted err [])
  where
  go (start, count) = do
    if index >= start && index < start + count
      then do
        -- that is our section, lets seek to the row
        Buffer.tell buf
          >>= Buffer.seek buf . (+ (fromIntegral $ index - start) * 20)
        (off, gen', free) <-
          Streams.parseFromStream parseTableEntry (Buffer.toInputStream buf)
            `catch` \(Streams.ParseException msg) ->
              throwIO (Corrupted "parseTableEntry failed" [msg])
        unless (free || gen == gen') $ do
          print (index, gen, off, gen', free)
          throwIO $ Corrupted "Generation mismatch" []
        let entry = if free
              then EntryFree (fromIntegral off) gen
              else EntryUsed off gen
        return (Just entry)
      else
        -- go to the next section if any
        nextSubsectionHeader (Buffer.toInputStream buf) count
        >>= maybe (return Nothing) go
lookupTableEntry _ XRefStream{} _ =
  throwIO $ Unexpected "lookupTableEntry" ["Only xref table allowed"]

-- | Read xref entry for the indirect object from xref stream
--
-- See pdf1.7 spec: 7.5.8 Cross-Reference Streams.
-- May throw 'UnknownXRefStreamEntryType'
lookupStreamEntry
  :: Dict                    -- ^ xref stream dictionary
  -> InputStream ByteString  -- ^ decoded xref stream content
  -> Ref                     -- ^ indirect object
  -> IO (Maybe Entry)
lookupStreamEntry dict is (R objNumber _) =
  message "lookupStreamEntry" $ do

  index <- sure $ do
    sz <- (HashMap.lookup "Size" dict >>= intValue)
      `notice` "Size should be an integer"
    i <-
      case HashMap.lookup "Index" dict of
        Nothing           -> Right [Number 0, Number (fromIntegral sz)]
        Just (Array arr) -> Right (Vector.toList arr)
        _                 -> Left "Index should be an array"

    let convertIndex res [] = Right (reverse res)
        convertIndex res (x1:x2:xs) = do
          from <- intValue x1 `notice` "from index should be an integer"
          count <- intValue x2 `notice` "count should be an integer"
          convertIndex ((from, count) : res) xs
        convertIndex _ _ = Left $ "Malformed Index in xref stream: " ++ show i

    convertIndex [] i

  width <- sure $ do
    ws <-
      case HashMap.lookup "W" dict of
        Just (Array ws) -> Right (Vector.toList ws)
        _ -> Left "W should be an array"
    mapM intValue ws
      `notice` "W should contains integers"

  unless (length width == 3) $
    throwIO $ Corrupted ("Malformed With array in xref stream: "
                        ++ show width) []

  values <- do
    let position = loop 0 index
        totalWidth = sum width
        loop _ [] = Nothing
        loop pos ((from, count) : xs) =
          if objNumber < from || objNumber >= from + count
            then loop (pos + totalWidth * count) xs
            else Just (pos + totalWidth * (objNumber - from))
    case position of
      Nothing -> return Nothing
      Just p -> do
        Buffer.dropExactly p is
        Just . ByteString.unpack <$> Streams.readExactly totalWidth is

  case values of
    Nothing -> return Nothing
    Just vs -> do
      let vs' = map conv $ collect [] width vs :: [Int64]
            where
            conv l = conv' (length l - 1) 0 l
            conv' _ res [] = res
            conv' power res (x:xs) =
              conv' (power-1) (res + (fromIntegral x * 256 ^ power)) xs
            collect res [] [] = reverse res
            collect res (x:xs) ys = collect (take x ys : res) xs (drop x ys)
            collect _ _ _ = error "readStreamEntry: collect: impossible"
      (v1, v2, v3) <- case vs' of
        [a, b, c] -> return (a, b, c)
        _ -> throwIO $ Corrupted "lookupStreamEntry" ["expected 3 values"]
      case v1 of
        0 -> return $ Just $ EntryFree (fromIntegral v2)
                                             (fromIntegral v3)
        1 -> return $ Just $ EntryUsed v2 (fromIntegral v3)
        2 -> return $ Just $ EntryCompressed (fromIntegral v2)
                                                   (fromIntegral v3)
        _ -> throwIO $ UnknownXRefStreamEntryType (fromIntegral v1)

-- | Unknown entry type should be interpreted as reference to null object
data UnknownXRefStreamEntryType = UnknownXRefStreamEntryType Int
  deriving (Show, Typeable)

instance Exception UnknownXRefStreamEntryType
