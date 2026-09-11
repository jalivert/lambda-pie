module SystemF.Substitute where

import SystemF.AST
import SystemF.Type
import SystemF.Name


subst'infer :: Int -> Term'Infer -> Term'Infer -> Term'Infer
subst'infer level rep (term ::: type')
  = subst'check level rep term ::: type'
subst'infer level rep (Bound indx name)
  | level == indx = rep
  | otherwise  = Bound indx name
subst'infer _ _ (Free name)
  = Free name
subst'infer level rep (left :@: right)
  = subst'infer level rep left :@: subst'check level rep right
subst'infer level rep (left :$: right't)
  = subst'infer level rep left :$: right't
subst'infer level rep (TyLam t'par term)
  = TyLam t'par $ subst'infer level rep term
subst'infer level rep (LamAnn par in'type body)
  = LamAnn par in'type $ subst'infer (level + 1) rep body


subst'check :: Int -> Term'Infer -> Term'Check -> Term'Check
subst'check level rep (Inf term)
  = Inf (subst'infer level rep term)
subst'check level rep (Lam par body)
  = Lam par (subst'check (level + 1) rep body)


subst'type :: Type -> String -> Type -> Type
subst'type (TFree (Global var)) name rep
  | var == name = rep
  | otherwise = TFree (Global var)
subst'type local@(TFree (Local _ _)) _ _
  = local
subst'type (from :-> to) name rep
  = subst'type from name rep :-> subst'type to name rep
subst'type (Forall par type') name rep
  | par == name = Forall par type'
  | otherwise = Forall par (subst'type type' name rep)
