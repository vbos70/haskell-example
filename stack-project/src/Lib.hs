{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Process (..),
      parseProcess
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
             | Alt Process Process
             | Rep Process
             | Error Text
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

parseProcess :: Text -> Process
parseProcess t =
  case parse processParser "" (unpack t) of
    Left e -> Error (pack (show e))
    Right p -> p

