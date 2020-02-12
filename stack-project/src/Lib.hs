{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Process (..),
      parseProcess,
      processToStr
    ) where

import System.IO
import Control.Monad
import Data.Text
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "Deadlock"
                                     , "Empty"
                                     ]
           , Token.reservedOpNames = ["|", ";", "*"
                                     ]
           }

lexer = Token.makeTokenParser languageDef


identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
whiteSpace = Token.whiteSpace lexer -- parses whitespace

procOperators = [ [Infix (reservedOp ";"   >> return (Seq )) AssocLeft ]
                , [Prefix (reservedOp "*"   >> return (Rep )) ]
                , [Infix (reservedOp "|"   >> return (Alt )) AssocLeft ]
                ]

data Process = Atom String
             | Empty
             | Deadlock
             | Seq Process Process
             | Rep Process
             | Alt Process Process
             | Error String
             deriving (Eq, Show)

processExpr :: Parser Process
processExpr = buildExpressionParser procOperators processTerm

processTerm = parens processExpr
           <|> (reserved "Deadlock"  >> return Deadlock)
           <|> (reserved "Empty"  >> return Empty)
           <|> liftM Atom identifier


processParser :: Parser Process
processParser =
  do whiteSpace
     p <- processExpr
     eof
     return p

parseProcess :: String -> Process
parseProcess t =
  case parse processParser "" t of
    Left e -> Error (show e)
    Right p -> p

priority :: Process -> Int
priority (Alt x y) = 3
priority (Rep x) = 2
priority (Seq x y) = 1
priority _ = 0

prefixOpProcessToParenStr :: String -> Process -> Int -> String 
prefixOpProcessToParenStr opStr x p = opStr ++ x_str1
  where
    prior_x = priority x
    x_str1 = processToParenStr x prior_x
    x_str2
      | prior_x > p = "(" ++ (x_str1) ++ ")"
      | otherwise   = x_str1

binOpProcessToParenStr :: Process -> String -> Process -> Int -> String 
binOpProcessToParenStr x opStr y p = x_str2 ++ opStr ++ y_str1
  where
    prior_x = priority x
    prior_y = priority y
    x_str1 = processToParenStr x prior_x
    x_str2
      | prior_x > p = "(" ++ (x_str1) ++ ")"
      | otherwise   = x_str1
    y_str1 = processToParenStr y prior_y
    y_str2
      | prior_y >= p = "(" ++ (y_str1) ++ ")"
      | otherwise    = y_str1

processToParenStr :: Process -> Int -> String
processToParenStr (Atom a) _ = a
processToParenStr (Error e) _ = e
processToParenStr Empty _    = "Empty"
processToParenStr Deadlock _ = "Deadlock"
processToParenStr (Seq x y) p = binOpProcessToParenStr x "; " y p
processToParenStr (Alt x y) p = binOpProcessToParenStr x " | " y p
processToParenStr (Rep x) p = prefixOpProcessToParenStr "*" x p


processToStr :: Process -> String
processToStr p = processToParenStr p (priority p)
