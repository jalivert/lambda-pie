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

-- The expected type applies the Pi closure to the very same fresh
-- variable that was substituted into the body above (as in the paper),
-- never to the Pi's own binder name: binder names are insignificant.
type'check level context (Lam par body) pi't@(Val.Pi _ in'type _ _) = do
    type'check  (level + 1)
                ((Local level par, in'type) : context)
                (subst'check 0 (Free (Local level par)) body)
                (val'app pi't (Val.Free (Local level par)))
type'check _ _ _ _ =
  throwError "Type mismatch. Incorrect shape."


-- Values compare up to alpha-equivalence. Binder names are insignificant:
-- closure bodies use de Bruijn indices, so bodies in equal environments
-- compare alpha-insensitively regardless of the bound names.
-- Closure bodies are open terms (index 0 is the closure's own binder),
-- so both sides are evaluated with their environments extended by one
-- shared fresh variable; leftover environment bindings beyond that do
-- not affect the result.
freshVar :: Val.Value
freshVar = Val.Free (Global "")

instance Eq Val.Value where
  (==) Val.Star Val.Star = True
  (==) (Val.Pi _ l'in'type l'body l'env) (Val.Pi _ r'in'type r'body r'env)
    | l'in'type == r'in'type
      = eval'check l'body (freshVar : l'env)
        == eval'check r'body (freshVar : r'env)
    | otherwise = False
  (==) (Val.Lam _ l'body l'env) (Val.Lam _ r'body r'env)
    = eval'check l'body (freshVar : l'env)
      == eval'check r'body (freshVar : r'env)

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
