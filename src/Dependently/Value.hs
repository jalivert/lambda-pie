module Dependently.Value where

import Dependently.AST (Term'Check)
import Dependently.Name (Name)


type Env = [Value]


-- NOTE: 'Free' keeps the full 'Name' (Global or level-keyed Local).
-- Erasing it to a bare String would conflate distinct binders that
-- happen to share a name, breaking alpha-invariance of typechecking.
data Value
  = Star
  | Pi String Value Term'Check Env
  | Lam String Term'Check Env
  | Free Name
  | App Value Value


instance Show Value where
  show Star
    = "*"
  show (Pi par in'type out'type [])
    = "(Π " ++ par ++ " :: " ++ show in'type ++ " . " ++ show out'type ++ ")"
  show (Pi par in'type out'type env)
    = "(Π " ++ par ++ " :: " ++ show in'type ++ " . " ++ show out'type ++ ")[" ++ show env ++ "]"
  show (Lam par body [])
    = "(λ " ++ par ++ " -> " ++ show body ++ ")"
  show (Lam par body env)
    = "(λ " ++ par ++ " -> " ++ show body ++ ")[" ++ show env ++ "]"
  show (Free name)
    = show name
  show (App left right)
    = "(" ++ show left ++ " @ " ++ show right ++ ")"
