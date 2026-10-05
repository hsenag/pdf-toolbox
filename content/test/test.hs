
module Main
(
  main
)
where

import qualified Test.UnicodeCMap
import qualified Test.Parser
import qualified Test.FontDescriptor
import qualified Test.Processor

import Test.Hspec

main :: IO ()
main = hspec $ do
  Test.UnicodeCMap.spec
  Test.Parser.spec
  Test.FontDescriptor.spec
  Test.Processor.spec
