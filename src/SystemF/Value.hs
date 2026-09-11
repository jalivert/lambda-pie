module SystemF.Value where

import SystemF.AST hiding (Lam, Free, TyLam)
import SystemF.Type


type Env = [Value]


data Value
  = Lam String Term'Check Env
  | TyLam String Term'Infer Env
  | Free String
  | App Value Value
  | TyApp Value Type


instance Show Value where
  show (Lam _ _ _)
    = "<lambda>"
  show (TyLam _ _ _)
    = "<type lambda>"
  show (Free name)
    = name
  show (App left right)
    = "(" ++ show left ++ " " ++ show right ++ ")"
  show (TyApp left right't) -- NEW
    = "(" ++ show left ++ " [" ++ show right't ++ "])" -- NEW
