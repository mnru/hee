{-# LANGUAGE OverloadedStrings #-}

module Language.Hee.Parser
  ( parseExpr
  , parseTerm
  , parseQuote
  , parseChar
  , parseString
  , parseNumber
  , parseName
  ) where

import Prelude hiding (takeWhile)
import Control.Applicative
import Data.Char (isSpace)
import Data.Text (cons, pack)
import Data.Attoparsec.Text

import Language.Hee.Terms
import Language.Hee.Simplify

parseExpr
  = simplify <$> (skipSpace *> scan)
  where
    scan  = TmCompose <$> parseTerm <*> ((space *> scan) <|> parseEmpty)
    space = takeWhile1 isSpace

parseTerm
  =   parseQuote   -- [
  <|> parseChar    -- '
  <|> parseString  -- "
  <|> parseNumber  -- 0
  <|> parseName    -- a
  <|> parseEmpty

parseQuote
  = parenthesized open inside close
  where
    open   = (char '[' *> skipSpace)
    close  = (skipSpace <* char ']')
    inside = TmQuote <$> parseExpr

parseEmpty
  = pure TmEmpty

parseChar
  = TmLiteral . LiChar <$> (delim *> (escapedChar <|> anyChar))
  where
    delim = char '\''

parseNumber
  = TmLiteral . LiNumber <$> signed number
  where
    number = (string "0x" *> hexadecimal) <|> decimal

parseString
  = TmLiteral . LiString . pack <$> (delim *> inside)
  where
    delim  = char '"'
    inside = manyTill (escapedChar <|> anyChar) delim

parseName
  = TmName <$> (cons <$> satisfy startChar <*> takeWhile otherChar)
  where
    startChar = notInClass " \t\r\n\f\v[]\"'"
    otherChar = notInClass " \t\r\n\f\v[]"

parenthesized open inside close
  = open *> inside <* close

escapedChar
  = tx <$> char '\\' *> anyChar
  where
    tx 'n'  = '\n'
    tx 't'  = '\t'
    tx 'r'  = '\r'
    tx '\\' = '\\'
    tx char = char
