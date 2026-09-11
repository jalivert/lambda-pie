module Main where

import Control.Exception (SomeException, evaluate, try)
import System.IO
import SystemF.AST (Term'Check)
import SystemF.Parser.Parser (parse'expr)
import SystemF.Eval
import SystemF.TypeChecker
import SystemF.Value
import SystemF.Context
import SystemF.Command

main :: IO ()
main = do
  putStrLn "REPL for λ2"
  putStrLn ""
  repl []


readExpression :: IO String
readExpression = do
  putStr "λ2 >> "
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
              repl $ assumptions ++ context
            Right expr -> do
              case type'of expr context of
                Left err -> do putStrLn $ "      Type Error: " ++ err
                Right type' -> do
                  let val = eval expr
                  putStrLn $ "      " ++ show val ++ " :: " ++ show type'
              repl context