module SimplyParserSpec where

import Test.Hspec
import Data.Either (isLeft, isRight)
import Control.Exception (evaluate)

import Simply.Parser.Parser (parse'expr)
import Simply.AST
import Simply.Name
import Simply.Type


spec :: Spec
spec = describe "Simply typed parser" $ do
  describe "parses terms to the expected AST" $ do
    it "a" $ do
      "a" <=>
        Inf (Free (Global "a"))

    it "a :: T" $ do
      "a :: T" <=>
        Inf (Inf (Free (Global "a")) ::: TFree (Global "T"))

    it "(a :: T)" $ do
      "(a :: T)" <=>
        Inf (Inf (Free (Global "a")) ::: TFree (Global "T"))

    it "((\\ a -> a) :: T -> T)" $ do
      "((\\ a -> a) :: T -> T)" <=>
        Inf ((Lam "a" (Inf (Bound 0 "a")))
             ::: (TFree (Global "T") :-> TFree (Global "T")))

  describe "parses REPL commands" $ do
    it "assume (T :: *)" $ do
      parse'expr "assume (T :: *)" `shouldSatisfy` isLeft

    it "assume (id :: T -> T)" $ do
      parse'expr "assume (id :: T -> T)" `shouldSatisfy` isLeft

  describe "rejects malformed input" $ do
    it "unbalanced paren raises" $ do
      evaluate (parse'expr "((\\ a -> a)") `shouldThrow` anyException

  describe "just parses" $ do
    it "a" $ do
      just'parses "a"
    it "a b c" $ do
      just'parses "a b c"
    it "(a b c)" $ do
      just'parses "(a b c)"
    it "(a :: T)" $ do
      just'parses "(a :: T)"
    it "a :: T" $ do
      just'parses "a :: T"
    it "(\\ a -> a)" $ do
      just'parses "(\\ a -> a)"
    it "(\\ a -> a) :: T -> T" $ do
      just'parses "((\\ a -> a) :: T -> T)"
    it "((\\ a -> a) :: T -> T)" $ do
      just'parses "((\\ a -> a) :: T -> T)"
    it "((\\ a -> a) :: (T -> T))" $ do
      just'parses "((\\ a -> a) :: (T -> T))"
    it "((\\ a -> a) :: T -> T) b" $ do
      just'parses "((\\ a -> a) :: T -> T) b"
    it "(\\ a -> a) :: T -> T b" $ do
      just'parses "(\\ a -> a) :: T -> T b"
    it "((\\ a -> a) :: T -> T) b c d" $ do
      just'parses "((\\ a -> a) :: T -> T) b c d"
    it "((\\ a -> a) :: T -> T) (b :: T) c d" $ do
      just'parses "((\\ a -> a) :: T -> T) (b :: T) c d"


just'parses :: String -> IO ()
just'parses expr =
  parse'expr expr `shouldSatisfy` isRight


infix 4 <=>

(<=>) :: String -> Term'Check -> IO ()
(<=>) expr ast = do
  case parse'expr expr of
    Left _ -> expectationFailure ("expected a term, got a command: " ++ expr)
    Right ast' -> ast' `shouldBe` ast
