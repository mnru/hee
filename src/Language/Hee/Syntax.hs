{-# LANGUAGE OverloadedStrings #-}

module Language.Hee.Syntax
  ( parseExpr     -- Parser Term
  , parseTerm     -- Parser Term
  , parseQuote    -- Parser Term
  , parseName     -- Parser Term

  , parseLiteral  -- Parser Literal
  , parseChar     -- Parser Literal
  , parseString   -- Parser Literal
  , parseNumber   -- Parser Literal

  , showKind      -- Kind    -> String
  , showType      -- Type    -> String
  , showTerm      -- Term    -> String
  , showLiteral   -- Literal -> String
  ) where

import Prelude hiding (takeWhile)
import Control.Applicative hiding (empty)
import Data.Char (showLitChar, isSpace, isPrint, chr, ord)
import Data.Text (cons, pack, unpack, empty)
import Data.Attoparsec.Text

import Language.Hee.Types
import Language.Hee.Terms
import Language.Hee.Simplify

------------------------------------------------------------------------------

instance Read Kind where
  readsPrec _ s
    = undefined

instance Read Type where
  readsPrec _ s
    = undefined

instance Read Term where
  readsPrec _ s
    = case result of
        Done xs x -> [(x, unpack xs)]
        _         -> []
    where result = feed (parse parseExpr $ pack s) empty

instance Read Literal where
  readsPrec _ s
    = case result of
        Done xs x -> [(x, unpack xs)]
        _         -> []
    where result = feed (parse parseLiteral $ pack s) empty

------------------------------------------------------------------------------

instance Show Kind where
  show = showKind

instance Show Type where
  show  = showType

instance Show Term where
  show = showTerm

instance Show Literal where
  show = showLiteral

showKind :: Kind -> String
showKind _ = undefined

showType :: Type -> String
showType _ = undefined

showTerm :: Term -> String
showTerm TmEmpty            = ""
showTerm (TmName x)         = unpack x
showTerm (TmQuote x)        = "[" ++ showTerm x ++ "]"
showTerm (TmLiteral x)      = showLiteral x
showTerm (TmCompose x y)    = showTerm x ++ " " ++ showTerm y
showTerm (TmAnnotation _ _) = undefined
showTerm (TmComment _)      = undefined

showLiteral :: Literal -> String
showLiteral (LiString x) = show $ unpack x
showLiteral (LiNumber x) = show x
showLiteral (LiChar x)   = escape x
  where
    escape '\n' = "'\\n"
    escape '\r' = "'\\r"
    escape '\t' = "'\\t"
    escape '\\' = "'\\\\"
    escape x | isPrint x = '\'' : [x]
    escape x | otherwise = '\'' : '\\' : show (ord x)

------------------------------------------------------------------------------

parseExpr :: Parser Term
parseExpr
  = simplify <$> (skipSpace *> scan)
  where
    scan  = TmCompose <$> parseTerm <*> ((space *> scan) <|> parseEmpty)
    space = takeWhile1 isSpace

parseTerm :: Parser Term
parseTerm
  =   parseQuote
  <|> TmLiteral <$> parseLiteral
  <|> parseName
  <|> parseEmpty

parseLiteral :: Parser Literal
parseLiteral
  =   parseChar
  <|> parseString
  <|> parseNumber

parseChar :: Parser Literal
parseChar
  = LiChar <$> (delim *> (escapedChar <|> anyChar))
  where
    delim = char '\''

parseNumber :: Parser Literal
parseNumber
  = LiNumber <$> signed number
  where
    number = (string "0x" *> hexadecimal) <|> decimal

parseString :: Parser Literal
parseString
  = LiString . pack <$> (delim *> inside)
  where
    delim  = char '"'
    inside = manyTill (escapedChar <|> anyChar) delim

parseName :: Parser Term
parseName
  = TmName <$> (cons <$> satisfy startChar <*> takeWhile otherChar)
  where
    startChar = notInClass " \t\r\n\f\v[]\"'"
    otherChar = notInClass " \t\r\n\f\v[]"

parseQuote :: Parser Term
parseQuote
  = parenthesized open inside close
  where
    open   = (char '[' *> skipSpace)
    close  = (skipSpace <* char ']')
    inside = TmQuote <$> parseExpr

parseEmpty :: Parser Term
parseEmpty
  = pure TmEmpty

parenthesized :: Applicative f => f a -> f b -> f c -> f b
parenthesized open inside close
  = open *> inside <* close

escapedChar :: Parser Char
escapedChar
  =   nn <$> (char '\\' *> decimal)
  <|> tx <$> (char '\\' *> anyChar)
  where
    nn num  = chr num
    tx 'n'  = '\n'
    tx 't'  = '\t'
    tx 'r'  = '\r'
    tx '\\' = '\\'
    tx char = char
