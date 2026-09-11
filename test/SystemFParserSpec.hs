module SystemFParserSpec where

import Test.Hspec
import Data.Either (isLeft, isRight)
import Control.Exception (evaluate)

import SystemF.Parser.Parser (parse'expr)
import SystemF.AST
import SystemF.Name
import SystemF.Type


spec :: Spec
spec = describe "System F parser" $ do
  describe "parses terms to the expected AST" $ do
    it "(/\\ T . (\\ (t :: T) -> t))" $ do
      "(/\\ T . (\\ (t :: T) -> t))" <=>
        Inf (TyLam "T" (LamAnn "t" (TFree (Global "T")) (Bound 0 "t")))

    it "id [Nat]" $ do
      "id [Nat]" <=>
        Inf (Free (Global "id") :$: TFree (Global "Nat"))

  describe "parses REPL commands" $ do
    it "assume (Nat :: *)" $ do
      parse'expr "assume (Nat :: *)" `shouldSatisfy` isLeft

    it "assume (id :: forall T . T -> T)" $ do
      parse'expr "assume (id :: forall T . T -> T)" `shouldSatisfy` isLeft

  describe "rejects malformed input" $ do
    it "truncated type lambda raises" $ do
      evaluate (parse'expr "(/\\ T .") `shouldThrow` anyException

  describe "just parses" $ do
    it "(/\\ T . (\\ (t :: T) -> t))" $ do
      just'parses "(/\\ T . (\\ (t :: T) -> t))"
    it "((/\\ T . (\\ (t :: T) -> t)) [Nat])" $ do
      just'parses "((/\\ T . (\\ (t :: T) -> t)) [Nat])"
    it "(\\ (x :: Nat) -> x)" $ do
      just'parses "(\\ (x :: Nat) -> x)"
    it "id [Nat]" $ do
      just'parses "id [Nat]"
    it "(pickone fst snd)" $ do
      just'parses "(pickone fst snd)"


just'parses :: String -> IO ()
just'parses expr =
  parse'expr expr `shouldSatisfy` isRight


infix 4 <=>

(<=>) :: String -> Term'Check -> IO ()
(<=>) expr ast = do
  case parse'expr expr of
    Left _ -> expectationFailure ("expected a term, got a command: " ++ expr)
    Right ast' -> ast' `shouldBe` ast
