{-# LANGUAGE OverloadedStrings #-}

module Language.Hee.Parser
  ( heeExpr
  , heeTerm
  , heeQuote
  , heeChar
  , heeString
  , heeNumber
  , heeName
  ) where

import Control.Applicative
import Prelude hiding (takeWhile)
import Data.Char (isSpace)
import Data.Text (cons, pack)
import Data.Attoparsec.Text
import Language.Hee.Terms

heeExpr
  = composeWith TmEmpty
  where
    whiteSpace    = takeWhile1 isSpace <?> "whiteSpace"
    composeWith t = if' <$> atEnd <*> pure t <*> composeNext t
    composeNext t = composeWith . (TmCompose t) =<< whiteSpace *> heeTerm

heeTerm
  =   (heeQuote  <?>  "quote") -- [
  <|> (heeChar   <?>   "char") -- '
  <|> (heeString <?> "string") -- "
  <|> (heeNumber <?> "number") -- 0
  <|> (heeName   <?>   "name") -- a

heeQuote 
  = parenthesized open inside close
  where
    open   = (char '[' <* skipSpace)
    close  = (skipSpace *> char ']')
    inside = TmQuote <$> heeExpr

heeChar
  = TmLiteral . LiChar <$> (char '\'' *> (escapedChar <|> anyChar))

heeString
  = parenthesized delim inside delim
  where
    delim  = char '"'
    inside = TmLiteral . LiString . pack <$> manyTill (escapedChar <|> anyChar) delim

heeNumber
  = TmLiteral . LiNumber <$> signed number
  where
    number = (string "0x" *> hexadecimal) <|> decimal

heeName
  = TmName <$> (cons <$> satisfy startChar <*> takeWhile otherChar)
  where
    startChar = notInClass " \t\r\n\f\v[]\"'"
    otherChar = notInClass " \t\r\n\f\v[]"

-- Private helpers
------------------

if' cond a b  = if cond then a else b

parenthesized open inside close
  = open *> inside <* close

escapedChar = tx <$> char '\\' *> anyChar
  where
    tx 'n'  = '\n'
    tx 't'  = '\t'
    tx 'r'  = '\r'
    tx '\\' = '\\'
    tx char = char
