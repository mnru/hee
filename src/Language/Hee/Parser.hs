{-# LANGUAGE OverloadedStrings #-}

module Language.Hee.Parser
  ( heeExpr
  , heeTerm
  ) where

import Control.Applicative
import Prelude hiding (takeWhile)
import Data.Text (cons, pack)
import Data.Attoparsec.Text
import Language.Hee.Terms

heeExpr
  = composeWith TmEmpty
  where
    composeWith t = skipSpace *> (if' <$> atEnd <*> pure t <*> composeNext t)
    composeNext t = heeTerm >>= composeWith . (TmCompose t)

heeTerm
  =   heeQuote    -- [
  <|> heeChar     -- '
  <|> heeString   -- "
  <|> heeNumber   -- 0
  <|> heeName     -- a

heeQuote 
  = parenthesized open inside close
  where
    open   = (char '[' <* skipSpace)
    close  = (skipSpace *> char ']')
    inside = TmQuote <$> heeExpr

heeChar
  = TmLiteral . LiChar <$> (char '\'' *> escapedChar <|> anyChar)

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
