{
module Dependently.Parser.Parser (parse'expr) where

import Data.List (elemIndex)
import Data.Bifunctor (first)

import Control.Monad (unless, fail)
import Control.Monad.State hiding (fix)

import qualified Dependently.Parser.Token as Tok
import Dependently.Parser.Lexer
import Dependently.Parser.Utils

import Dependently.Command
import Dependently.AST
import Dependently.Name
import Dependently.Context
}


%name parserAct
%tokentype { Tok.Token }
%error { parseError }
%monad { P }
%lexer { lexer } { Tok.EOF }


%token
  assume        { Tok.Assume }
  var           { Tok.Identifier $$ }
  lambda        { Tok.Lambda }
  '::'          { Tok.DoubleColon }
  '('           { Tok.LeftParen }
  ')'           { Tok.RightParen }
  '*'           { Tok.Star }
  '->'          { Tok.Arrow }
  '.'           { Tok.Dot }
  pi            { Tok.Forall }


%%
Program         ::  { Either Command Term'Check }
                :   Term                                            { Right $1 }
                |   Command                                         { Left $1 }


TypedParams     ::  { [(String, Term'Check)] }
                :   var '::' Type                                   { [($1, $3)] }
                |   OneOrMany(TypedParam)                           { $1 }


TypedParam      ::  { (String, Term'Check) }
                :   '(' var '::' Type ')'                           { ($2, $4) }


App             ::  { Term'Infer }
                :   AppLeft OneOrMany(AppRight)                     { foldl (:@:) $1 $2 }


AppLeft         ::  { Term'Infer }
                :   '*'                                             { Star }
                |   Pi                                              { $1 }
                |   var                                             { Free $ Global $1 }
                |   var '::' Type                                   { Inf (Free $ Global $1) ::: $3 }
                |   '(' var '::' Type ')'                           { Inf (Free $ Global $2) ::: $4 }
                |   '(' Lambda '::' Type ')'                        { $2 ::: $4 }
                |   Lambda '::' Type                                { $1 ::: $3 }
                |   '(' App ')'                                     { $2 }
                |   '(' App ')' '::' Type                           { Inf $2 ::: $5 }
                |   '(' '(' App ')' '::' Type ')'                   { Inf $3 ::: $6 }


AppRight        ::  { Term'Check }
                :   '*'                                             { Inf $ Star }
                |   Pi                                              { Inf $ $1 }
                |   Lambda                                          { $1 }
                |   '(' Lambda '::' Type ')'                        { Inf $ $2 ::: $4 }
                -- |   Lambda '::' Type                                { Inf $1 ::: $3 }
                |   var                                             { Inf $ Free $ Global $1 }
                |   '(' var '::' Type ')'                           { Inf (Inf (Free $ Global $2) ::: $4) }
                -- |   var '::' Type                                   { Inf (Inf (Free $ Global $1) ::: $3) }
                |   '(' App ')'                                     { Inf $2 }
                -- |   '(' App ')' '::' Type                           { Inf (Inf $2 ::: $5) }
                |   '(' '(' App ')' '::' Type ')'                   { Inf (Inf $3 ::: $6) }


Term            ::  { Term'Check }
                :   Term '::' Type                                  { Inf $ $1 ::: $3 }
                |   '*'                                             { Inf Star }
                |   pi TypedParams '.' Type                         { Inf $ fix $ unwrap $ foldr
                                                                        (\ (name, type') body ->
                                                                          case body of
                                                                          { Check ch -> Infer $ Pi name type' ch
                                                                          ; Infer i -> Infer $ Pi name type' $ Inf i })
                                                                        (Check $4)
                                                                        $2 }
                |   var                                             { Inf $ Free $ Global $1 }
                |   '(' App ')'                                     { Inf $2 }
                |   Lambda                                          { $1 }
                |   '(' Term ')'                                    { $2 }


Type            ::  { Term'Check }
                :   Term                                            { $1 }


Pi                 ::  { Term'Infer }
                   :   pi TypedParams '.' Term                         { fix $ unwrap $ foldl
                                                                        (\ body (name, type') ->
                                                                          case body of
                                                                          { Check ch -> Infer $ Pi name type' ch
                                                                          ; Infer i -> Infer $ Pi name type' $ Inf i })
                                                                        (Check $4)
                                                                        $2 }


Params          ::  { [String] }
                :   OneOrMany(var)                                  { $1 }


Lambda          ::  { Term'Check }
                :   '(' lambda Params '->' Term ')'                 { fix $ foldr
                                                                     (\ arg body -> Lam arg body)
                                                                     $5
                                                                     $3 }


Command         ::  { Command }
                :   assume TypedParams                              { Assume $ map (first Global) $2 }




NoneOrMany(tok)
                :   {- empty -}                                     { [] }
                |   tok NoneOrMany(tok)                             { $1 : $2 }

OneOrMany(tok)
                :   tok NoneOrMany(tok)                             { $1 : $2 }

{

data Inf'or'Check
  = Infer Term'Infer
  | Check Term'Check


unwrap :: Inf'or'Check -> Term'Infer
unwrap (Infer inf) = inf


class Fix a where
  fix :: a -> a


instance Fix Term'Check where
  fix lambda = fix'check lambda []


instance Fix Term'Infer where
  fix lambda = fix'infer lambda []


fix'check :: Term'Check -> [String] -> Term'Check
fix'check (Lam par body) context
  = Lam par $ fix'check body (par : context)
fix'check (Inf term) context
  = Inf $ fix'infer term context


fix'infer :: Term'Infer -> [String] -> Term'Infer
fix'infer (term ::: type') context
  = (fix'check term context) ::: (fix'check type' context)
fix'infer Star _
  = Star
-- The domain of a Pi cannot mention the bound parameter, so only the
-- codomain is resolved with the parameter in scope.
fix'infer (Pi par in'type out'type) context
 = Pi par (fix'check in'type context) (fix'check out'type (par : context))
fix'infer (Bound i n) _
  = Bound i n
fix'infer (Free (Global name)) context
  = case elemIndex name context of
      Just ind -> Bound ind name
      Nothing -> Free (Global name)
fix'infer (left :@: right) context
  = (fix'infer left context) :@: (fix'check right context)


parseError _ = do
  lno <- getLineNo
  colno <- getColNo
  s <- get
  error $ "Parse error on line " ++ show lno ++ ", column " ++ show colno ++ "." ++ "\n" ++ (show s)


parse'expr :: String -> Either Command Term'Check
parse'expr s =
  evalP parserAct s
}
