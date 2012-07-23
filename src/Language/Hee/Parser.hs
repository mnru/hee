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
  = simplify <$> (skipSpace *> scan)
  where
    scan  = TmCompose <$> heeTerm <*> ((space *> scan) <|> heeEmpty)
    space = takeWhile1 isSpace

heeTerm
  =   heeQuote   -- [
  <|> heeChar    -- '
  <|> heeString  -- "
  <|> heeNumber  -- 0
  <|> heeName    -- a
  <|> heeEmpty

heeQuote 
  = parenthesized open inside close
  where
    open   = (char '[' *> skipSpace)
    close  = (skipSpace <* char ']')
    inside = TmQuote <$> heeExpr

heeChar
  = TmLiteral . LiChar <$> (char '\'' *> (escapedChar <|> anyChar))

heeString
  = TmLiteral . LiString . pack <$> (delim *> inside)
  where
    delim  = char '"'
    inside = manyTill (escapedChar <|> notChar '"') delim

heeNumber
  = TmLiteral . LiNumber <$> signed number
  where
    number = (string "0x" *> hexadecimal) <|> decimal

heeName
  = TmName <$> (cons <$> satisfy startChar <*> takeWhile otherChar)
  where
    startChar = notInClass " \t\r\n\f\v[]\"'"
    otherChar = notInClass " \t\r\n\f\v[]"

heeEmpty
  = pure TmEmpty

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
