module SystemFTypeCheckSpec where

import Test.Hspec
import Data.Either (isLeft)

import SystemF.Parser.Parser (parse'expr)
import SystemF.TypeChecker (type'of)
import SystemF.Eval (eval)
import SystemF.Context (Context, Info (..))
import SystemF.Name (Name (..))
import SystemF.Kind (Kind (..))
import SystemF.Type (Type (..))


tVar :: String -> Type
tVar = TFree . Global


testContext :: Context
testContext =
  [ (Global "Nat", HasKind Star)
  , (Global "id", HasType (Forall "T" (tVar "T" :-> tVar "T")))
  , (Global "x", HasType (tVar "Nat"))
  ]


spec :: Spec
spec = describe "System F typechecker" $ do
  describe "infers types" $ do
    it "(/\\ T . (\\ (t :: T) -> t)) :: forall T . T -> T" $ do
      type'checks testContext
        "(/\\ T . (\\ (t :: T) -> t))"
        (Forall "T" (tVar "T" :-> tVar "T"))

    it "((/\\ T . (\\ (t :: T) -> t)) [Nat]) :: Nat -> Nat" $ do
      type'checks testContext
        "((/\\ T . (\\ (t :: T) -> t)) [Nat])"
        (tVar "Nat" :-> tVar "Nat")

    it "id [Nat] :: Nat -> Nat" $ do
      type'checks testContext "id [Nat]" (tVar "Nat" :-> tVar "Nat")

    it "(\\ (x :: Nat) -> x) :: Nat -> Nat" $ do
      type'checks testContext
        "(\\ (x :: Nat) -> x)"
        (tVar "Nat" :-> tVar "Nat")

  describe "rejects ill-typed terms" $ do
    it "(\\ (x :: Nat) -> x) with Nat unknown" $ do
      type'rejects [] "(\\ (x :: Nat) -> x)"

    it "type application of a term lambda" $ do
      type'rejects testContext "((\\ (x :: Nat) -> x) [Nat])"

    it "(/\\ T . (\\ (t :: U) -> t)) with U unknown" $ do
      type'rejects [] "(/\\ T . (\\ (t :: U) -> t))"

  describe "evaluates" $ do
    it "type application erases to a lambda" $ do
      case parse'expr "((/\\ T . (\\ (t :: T) -> t)) [Nat])" of
        Left _ -> expectationFailure "expected a term, got a command"
        Right ast -> show (eval ast) `shouldBe` "<lambda>"


type'checks :: Context -> String -> Type -> IO ()
type'checks context expr expected =
  case parse'expr expr of
    Left _ -> expectationFailure ("expected a term, got a command: " ++ expr)
    Right ast -> type'of ast context `shouldBe` Right expected


type'rejects :: Context -> String -> IO ()
type'rejects context expr =
  case parse'expr expr of
    Left _ -> expectationFailure ("expected a term, got a command: " ++ expr)
    Right ast -> type'of ast context `shouldSatisfy` isLeft
