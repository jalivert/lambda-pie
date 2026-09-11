module SystemF.Parser.Utils where

import Control.Monad.State
import Data.Word
import Codec.Binary.UTF8.String (encode)


-- Parser monad
type P a = State ParseState a


data AlexInput = AlexInput
  { ai'prev :: Char
  , ai'bytes :: [Word8]
  , ai'rest :: String
  , ai'line'number :: Int
  , ai'column'number :: Int }
  deriving Show


data ParseState = ParseState
  { input :: AlexInput
  , lexSC :: Int }                    -- lexer start code
  deriving Show


initialState :: String -> ParseState
initialState s = ParseState
  { input = AlexInput
    { ai'prev = '\n'
    , ai'bytes = []
    , ai'rest = s
    , ai'line'number = 1
    , ai'column'number = 1 }
  , lexSC = 0 }


-- The functions that must be provided to Alex's basic interface

-- The input: last character, unused bytes, remaining string
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte ai =
  case ai'bytes ai of
    (b : bs) ->
      Just (b, ai{ ai'bytes = bs })

    [] ->
      case ai'rest ai of
        [] -> Nothing

        (char : chars) ->
          let
            n = ai'line'number ai
            n' = if char == '\n' then n + 1 else n
            c = ai'column'number ai
            c' = if char == '\n' then 1 else c + 1
            advance b bs =
              Just (b, AlexInput  { ai'prev = char
                                  , ai'bytes = bs
                                  , ai'rest = chars
                                  , ai'line'number = n'
                                  , ai'column'number = c' })
          -- The UTF-8 encoding of a single Char is never empty;
          -- the fallback only keeps the match total.
          in case encode [char] of
            (b : bs) -> advance b bs
            [] -> Nothing


getLineNo :: P Int
getLineNo = do
  s <- get
  return . ai'line'number . input $ s


getColNo :: P Int
getColNo = do
  s <- get
  return . ai'column'number . input $ s


evalP :: P a -> String -> a
evalP m s = evalState m (initialState s)
