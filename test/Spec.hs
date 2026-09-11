import Test.Hspec

import qualified DepParserSpec
import qualified DepTypeCheckSpec

import qualified SimplyParserSpec
import qualified SimplyTypeCheckSpec

import qualified SystemFParserSpec
import qualified SystemFTypeCheckSpec


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Simply Typed Parsing Test" SimplyParserSpec.spec
  describe "Simply Typed Typechecking Test" SimplyTypeCheckSpec.spec

  describe "System F Parsing Test" SystemFParserSpec.spec
  describe "System F Typechecking Test" SystemFTypeCheckSpec.spec

  describe "Dependently Typed Parsing Test" DepParserSpec.spec
  describe "Dependently Typed Typechecking Test" DepTypeCheckSpec.spec
