module SimplyTypeCheckSpec where

import Test.Hspec
import Data.Either (isLeft)

import Simply.Parser.Parser (parse'expr)
import Simply.TypeChecker (type'of)
import Simply.Eval (eval)
import Simply.Context (Context, Info (..))
import Simply.Name (Name (..))
import Simply.Kind (Kind (..))
import Simply.Type (Type (..))


tVar :: String -> Type
tVar = TFree . Global


testContext :: Context
testContext =
  [ (Global "T", HasKind Star)
  , (Global "Bool", HasKind Star)
  , (Global "id", HasType (tVar "T" :-> tVar "T"))
  , (Global "a", HasType (tVar "T"))
  , (Global "b", HasType (tVar "Bool"))
  ]


spec :: Spec
spec = describe "Simply typed typechecker" $ do
  describe "infers types" $ do
    it "id :: T -> T" $ do
      type'checks testContext "id" (tVar "T" :-> tVar "T")

    it "((\\ (x :: T) -> x) (a :: T)) :: T" $ do
      type'checks testContext "((\\ (x :: T) -> x) (a :: T))" (tVar "T")

    it "((\\ a -> a) :: T -> T) :: T -> T" $ do
      type'checks testContext "((\\ a -> a) :: T -> T)" (tVar "T" :-> tVar "T")

  describe "rejects ill-typed terms" $ do
    it "(b :: U) with U unknown" $ do
      type'rejects testContext "(b :: U)"

    it "bare (\\ x -> x) cannot be inferred" $ do
      type'rejects testContext "(\\ x -> x)"

    it "applying T -> T to a Bool" $ do
      type'rejects testContext "((\\ (x :: T) -> x) (b :: Bool))"

    it "applying a non-function" $ do
      type'rejects testContext "(a (b :: T))"

  describe "evaluates" $ do
    it "((\\ (x :: T) -> x) (a :: T)) reduces to a" $ do
      case parse'expr "((\\ (x :: T) -> x) (a :: T))" of
        Left _ -> expectationFailure "expected a term, got a command"
        Right ast -> show (eval ast) `shouldBe` "a"


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
