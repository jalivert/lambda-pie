module SystemF.AST where

import SystemF.Type ( Type )
import SystemF.Name ( Name )


-- Inferable Term
data Term'Infer
  = Term'Check ::: Type
  | Bound Int String
  | Free Name
  | Term'Infer :@: Term'Check
  | Term'Infer :$: Type
  | TyLam String Term'Infer
  | LamAnn String Type Term'Infer
  deriving (Eq)


instance Show Term'Infer where
  show (term ::: type')
    = show term ++ " :: " ++ show type'
  show (Bound ind name)
    = name ++ show ind
  show (Free name)
    = show name
  show (left :@: r@(Inf (_ :@: _)))
    = show left ++ " (" ++ show r ++ ")"
  show (left :@: right)
    = show left ++ " " ++ show right
  show (left :$: t'right)
    = "(" ++ show left ++ " " ++ show t'right ++ ")"
  show (TyLam t'par body)
    = "(Λ " ++ t'par ++ " . " ++ show body ++ ")"
  show (LamAnn par in'type body)
    = "(λ " ++ par ++ " :: " ++ show in'type ++ " -> " ++ show body ++ ")"


-- Checkable Term
data Term'Check
  = Inf Term'Infer
  | Lam String Term'Check
  deriving (Eq)


instance Show Term'Check where
  show (Inf term)
    = show term
  show (Lam par body)
    = "(λ " ++ par ++ " -> " ++ show body ++ ")"