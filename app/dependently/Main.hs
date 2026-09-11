module Main where

import Control.Exception (SomeException, evaluate, try)
import System.IO
import Data.Bifunctor (second)

import Dependently.AST (Term'Check)
import Dependently.Parser.Parser (parse'expr)
import Dependently.Context ( Context )
import Dependently.Eval ( Evaluate(eval) )
import Dependently.TypeChecker ( Typeable(type'of) )
import Dependently.Command ( Command(Assume) )

main :: IO ()
main = do
  putStrLn "REPL for λΠ"
  putStrLn ""
  repl []

readExpression :: IO String
readExpression = do
  putStr "λΠ >> "
  hFlush stdout
  getLine


repl :: Context -> IO ()
repl context = do
  line <- readExpression
  if line == ":exit"
    then return ()
    else do
      parsed <- try (evaluate (parse'expr line)) :: IO (Either SomeException (Either Command Term'Check))
      case parsed of
        Left err -> do
          putStrLn $ "      Parse Error: " ++ show err
          repl context
        Right command'or'expr ->
          case command'or'expr of
            Left (Assume assumptions) -> do
              repl $ map (second eval) assumptions ++ context
            Right expr -> do
              case type'of expr context of
                Left err -> do putStrLn $ "      Type Error: " ++ err
                Right type' -> do
                  let val = eval expr
                  putStrLn $ "      " ++ show val ++ " :: " ++ show type'
              repl context