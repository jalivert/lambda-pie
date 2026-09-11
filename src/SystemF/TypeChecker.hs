module SystemF.TypeChecker where

import Control.Monad

import SystemF.Context
import SystemF.Kind
import SystemF.Type
import SystemF.AST
import SystemF.Name
import SystemF.Substitute


type Result a = Either String a


throwError :: String -> Result a
throwError = Left


kind'check :: Context -> Type -> Kind -> Result ()
kind'check context (TFree name) Star =
  case lookup name context of
    Just (HasKind Star) -> return ()
    Just _ -> throwError $ "A type var " ++ show name ++ " isn't of a kind *."
    Nothing -> throwError $ "Unknown type var identifier " ++ show name ++ "."
kind'check context (left't :-> right't) Star = do
  kind'check context left't Star
  kind'check context right't Star
kind'check context (Forall t'par type') Star = do
  kind'check ((Global t'par, HasKind Star) : context) type' Star


type'infer'0 :: Context -> Term'Infer -> Result Type
type'infer'0 = type'infer 0


type'infer :: Int -> Context -> Term'Infer -> Result Type
type'infer level context (e ::: type') = do
  kind'check context type' Star
  type'check level context e type'
  return type'
type'infer _ context (Free name) = do
  case lookup name context of
    Just (HasType type') -> return type'
    Just _ -> throwError $ "Type error for " ++ show name ++ "."
    Nothing -> throwError "Unknown identifier."
type'infer level context (left :@: right) = do
  left't <- type'infer level context left
  case left't of
    in't :-> out't -> do
      type'check level context right in't
      return out't
    _ -> throwError "Type error: illegal application."
type'infer level context (TyLam t'par body) = do
  body't <- type'infer level ((Global t'par, HasKind Star) : context) body
  return $ Forall t'par body't
type'infer level context (left :$: t'right) = do
  kind'check context t'right Star
  left't <- type'infer level context left
  case left't of
    Forall t'par out'type -> do
      let res'type = subst'type out'type t'par t'right
      return res'type
    _ -> throwError "Type error: illegal type application."
type'infer level context (LamAnn par in'type body) = do
  kind'check context in'type Star
  out'type <- type'infer (level + 1) ((Local level par, HasType in'type) : context)
                (subst'infer 0 (Free (Local level par)) body)
  return $ in'type :-> out'type
type'infer _ _ _ =
  throwError "Type error: cannot infer the type of this term."


type'check :: Int -> Context -> Term'Check -> Type -> Result ()
type'check level context (Inf e) type' = do
  e't <- type'infer level context e
  unless (type' == e't) (throwError "Type mismatch.")
type'check level context (Lam par body) (in't :-> out't) = do
  type'check (level + 1) ((Local level par, HasType in't) : context)
            (subst'check 0 (Free (Local level par)) body) out't
type'check _ _ _ _ =
  throwError "Type mismatch."


class Typeable a where
  type'of :: a -> Context -> Result Type


instance Typeable Term'Infer where
  type'of ann@(_ ::: _) context
    = type'infer'0 context ann
  type'of app@(_ :@: _) context
    = type'infer'0 context app
  type'of t'app@(_ :$: _) context
    = type'infer'0 context t'app
  type'of t'lam@(TyLam _ _) context
    = type'infer'0 context t'lam
  type'of lam@(LamAnn _ _ _) context
    = type'infer'0 context lam
  type'of other context
    = type'infer'0 context other


instance Typeable Term'Check where
  type'of (Inf expr) context
    = type'of expr context
  type'of (Lam _ _) _
    = Left "Can't infer a type of an unannotated λ."
