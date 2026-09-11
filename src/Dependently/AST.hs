module Dependently.AST where

import Dependently.Name


-- Inferable Term
data Term'Infer
  = Term'Check ::: Term'Check
  | Star
  | Pi String Term'Check Term'Check
  | Bound Int String
  | Free Name
  | Term'Infer :@: Term'Check
  deriving (Eq)


instance Show Term'Infer where
  show (term ::: type')
    = show term ++ " :: " ++ show type'
  show Star
    = "*"
  show (Pi par in'type out'type)
    = "(Π " ++ par ++ " :: " ++ show in'type ++ " . " ++ show out'type ++ ")"
  show (Bound _ name)
    = name
  show (Free name)
    = show name
  show (left :@: r@(Inf (_ :@: _)))
    = show left ++ " (" ++ show r ++ ")"
  show (left :@: right)
    = show left ++ " " ++ show right


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