module Dependently.TypeChecker where

import Control.Monad

import Dependently.Context
import Dependently.AST
import qualified Dependently.Value as Val
import Dependently.Name
import Dependently.Eval
import Dependently.Substitute


type Result a = Either String a

throwError :: String -> Result a
throwError = Left


type'infer'0 :: Context -> Term'Infer -> Result Type
type'infer'0 = type'infer 0


type'infer :: Int -> Context -> Term'Infer -> Result Type
type'infer level context (e ::: type') = do
  type'check level context type' Val.Star
  let type'' = eval'check type' []
  type'check level context e type''
  return type''

type'infer _ _ Star =
  return Val.Star

type'infer level context (Pi par in'type out'type) = do
  type'check level context in'type Val.Star
  let in'type' = eval'check in'type []
  type'check (level + 1) ((Local level par, in'type') : context)
              (subst'check 0 (Free (Local level par)) out'type) Val.Star
  return Val.Star

type'infer _ context (Free name) = do
  case lookup name context of
    Just type' -> return type'
    Nothing ->
      throwError ("Unknown identifier " ++ show name ++ ".")

type'infer level context (left :@: right) = do
  left't <- type'infer level context left
  case left't of
    pi't@(Val.Pi _ in'type _ _) -> do
      type'check level context right in'type
      return $ val'app pi't (eval'check right [])
    _ -> throwError "Type error: illegal application! Type of *left* must be a Pi."

type'infer _ _ _ =
  throwError "Type error: cannot infer the type of this term."


type'check :: Int -> Context -> Term'Check -> Type -> Result ()
type'check level context (Inf e) type' = do
  e't <- type'infer level context e
  unless (type' == e't) (throwError $ "Type mismatch. type' = " ++ show type' ++ "  /= " ++ show e't ++ "\ncontext= " ++ show context)

type'check level context (Lam par body) pi't@(Val.Pi param in'type _ _) = do
    type'check  (level + 1)
                ((Local level par, in'type) : context)
                (subst'check 0 (Free (Local level par)) body)
                (val'app pi't (Val.Free param))
type'check _ _ _ _ =
  throwError "Type mismatch. Incorrect shape."


-- Values compare structurally; closures (Pi/Lam) compare by evaluating
-- their bodies in the captured environments first, so that leftover
-- environment bindings do not affect the result.
instance Eq Val.Value where
  (==) Val.Star Val.Star = True
  (==) (Val.Pi l'par l'in'type l'body l'env) (Val.Pi r'par r'in'type r'body r'env)
    | l'par == r'par && l'in'type == r'in'type
      = let
          l'val = eval'check l'body l'env
          r'val = eval'check r'body r'env
        in
          l'val == r'val
    | otherwise = False
  (==) (Val.Lam l'par l'body l'env) (Val.Lam r'par r'body r'env)
    | l'par == r'par
      = let
          l'val = eval'check l'body l'env
          r'val = eval'check r'body r'env
        in
          l'val == r'val
    | otherwise = False

  (==) (Val.Free l'id) (Val.Free r'id)
    = l'id == r'id
  (==) (Val.App l'left l'right) (Val.App r'left r'right)
    = l'left == r'left && l'right == r'right
  (==) _ _ = False


class Typeable a where
  type'of :: a -> Context -> Result Type


instance Typeable Term'Infer where
  type'of ann@(_ ::: _) context
    = type'infer'0 context ann
  type'of app@(_ :@: _) context
    = type'infer'0 context app
  type'of other context
    = type'infer'0 context other


instance Typeable Term'Check where
  type'of (Inf expr) context
    = type'of expr context
  type'of (Lam _ _) _
    = Left "Can't infer a type of an unannotated λ."
