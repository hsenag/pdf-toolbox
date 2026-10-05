{-# LANGUAGE OverloadedStrings #-}

module Test.Processor
(
  spec,
)
where

import Pdf.Core
import Pdf.Content.Ops
import Pdf.Content.Processor
import Pdf.Content.Transform

import Control.Monad (foldM)
import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "Processor" $ do
  actualTextSpec

actualTextSpec :: Spec
actualTextSpec = describe "ActualText" $ do
  it "should leave text outside marked content alone" $ do
    texts [tj "a"] `shouldBe` Right [[Just "\0"]]

  it "should replace the text of a glyph drawn in the section" $ do
    texts [actual "7", tj "a", emc, tj "b"]
      `shouldBe` Right [[Just "7"], [Just "\0"]]

  it "should give the text to the first glyph only" $ do
    texts [actual "12", tj "ab", tjArray ["c"], emc]
      `shouldBe` Right [[Just "12", Nothing], [Nothing]]

  it "should use the outermost ActualText" $ do
    texts [actual "x", bmc, tj "a", emc, actual "y", tj "b", emc, emc]
      `shouldBe` Right [[Just "x"], [Nothing]]

  it "should decode UTF-16BE" $ do
    let props = Dict $ HashMap.singleton "ActualText"
          (String "\254\255\0\163")
    texts [(Op_BDC, [Name "Span", props]), tj "a", emc]
      `shouldBe` Right [[Just "\163"]]

  it "should ignore properties that are named, not given inline" $ do
    texts [(Op_BDC, [Name "Span", Name "MC0"]), tj "a", emc]
      `shouldBe` Right [[Just "\0"]]

  it "should ignore an EMC with no section to end" $ do
    texts [emc, tj "a"] `shouldBe` Right [[Just "\0"]]
  where
  actual t = (Op_BDC, [Name "Span", actualTextDict t])
  bmc = (Op_BMC, [Name "Span"])
  emc = (Op_EMC, [])
  tj s = (Op_Tj, [String s])
  tjArray ss = (Op_TJ, [Array (Vector.fromList (map String ss))])

actualTextDict :: ByteString.ByteString -> Object
actualTextDict t = Dict (HashMap.singleton "ActualText" (String t))

-- | The text of the glyphs drawn by the operators, a list per span,
-- with a font whose every code decodes to U+0000
texts :: [Operator] -> Either String [[Maybe Text]]
texts ops = do
  p <- foldM (flip processOp) start (setup ++ ops)
  return (map (map glyphText . spGlyphs) (reverse (prSpans p)))
  where
  start = mkProcessor {prGlyphDecoder = decoder}
  setup = [(Op_BT, []), (Op_Tf, [Name "F1", Number 10])]
  decoder _ str =
    [ (Glyph (fromIntegral c) (Vector 0 0) (Vector 1 1) (Just "\0"), 1)
    | c <- ByteString.unpack str
    ]
