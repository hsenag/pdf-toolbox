
-- | Parse content stream

module Pdf.Content.Parser
(
  readNextOperator,
  parseContent,
)
where

import Pdf.Core.Exception
import Pdf.Core.Parsers.Object

import Pdf.Content.Ops

import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Attoparsec.Combinator (lookAhead)
import qualified Data.Attoparsec.ByteString.Char8 as Parser
import Control.Applicative
import Control.Monad
import Control.Exception hiding (throw)
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

-- | Read the next operator if any
readNextOperator :: InputStream Expr -> IO (Maybe Operator)
readNextOperator is = message "readNextOperator" $ go []
  where
  go args = do
    expr <- Streams.read is
      -- XXX: it should be handled by stream creator
      `catch` \(Streams.ParseException msg) -> throwIO (Corrupted msg [])
    case expr of
      Nothing -> case args of
                   [] -> return Nothing
                   _ -> throwIO $ Corrupted ("Args without op: " ++ show args) []
      Just (Obj o) -> go (o : args)
      Just (Op o) -> return $ Just (o, reverse args)

-- | Parser expression in a content stream
parseContent :: Parser (Maybe Expr)
parseContent = do
  skipSpace
  (Parser.endOfInput >> return Nothing) <|>
    fmap Just (fmap Obj parseObject <|> parseOperator)

parseOperator :: Parser Expr
parseOperator = do
  op <- toOp <$> Parser.takeWhile1 isRegularChar
  -- The data of an inline image follows ID as raw bytes. They are
  -- neither objects nor operators, so skip over them and leave the
  -- EI that ends the image to be read as the next operator.
  when (op == Op_ID) skipInlineImageData
  return (Op op)

-- | Skip the data of an inline image
--
-- It starts after the single whitespace character that follows ID, and
-- runs to the next EI that is delimited by whitespace. (The data can
-- contain anything at all, including something that looks like EI, so
-- this is a heuristic; it is the one the spec suggests for data whose
-- length isn't known.)
skipInlineImageData :: Parser ()
skipInlineImageData = Parser.satisfy isPdfSpace >> go
  where
  go = do
    Parser.skipWhile (not . isPdfSpace)
    atEnd <- Parser.atEnd
    unless atEnd $
      imageEnd <|> (void (Parser.satisfy isPdfSpace) >> go)
  -- leaves the EI in place, so that it is read as the next operator
  imageEnd = lookAhead $ do
    Parser.skipWhile isPdfSpace
    _ <- Parser.char 'E'
    _ <- Parser.char 'I'
    next <- Parser.peekChar
    case next of
      Nothing -> return ()
      Just c | isRegularChar c -> fail "not the end of an inline image"
             | otherwise -> return ()

isPdfSpace :: Char -> Bool
isPdfSpace c = c `elem` ("\0\t\n\f\r " :: String)

-- Treat comments as spaces
skipSpace :: Parser ()
skipSpace = do
  Parser.skipSpace
  void $ many $ do
    _ <- Parser.char '%'
    Parser.skipWhile $ \c -> c /= '\n' && c /= '\r'
    Parser.skipSpace
