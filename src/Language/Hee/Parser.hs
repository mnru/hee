{-# LANGUAGE OverloadedStrings #-}

module Language.Hee.Parser
  ( ParseError(..)
  , parseOnly
  , parseSome
  , parseMore
  , parseDone

  , parseCompose  -- Parser Expr
  , parseExpr     -- Parser Expr
  , parseQuote    -- Parser Expr
  , parseName     -- Parser Expr

  , parseLiteral  -- Parser Literal
  , parseChar     -- Parser Literal
  , parseString   -- Parser Literal
  , parseNumber   -- Parser Literal
  ) where

import Language.Hee.Syntax
import Language.Hee.Simplify

import Prelude hiding (takeWhile)
import Control.Applicative hiding (empty)
import Data.Bits (Bits, shiftL, (.|.))
import Data.Char (showLitChar, isSpace, isPrint, chr, ord)
import Data.Text (Text, cons, pack, unpack, empty, foldl')
import Data.Attoparsec.Text hiding (parseOnly, Partial)

data ParseError a
  = Partial a Text
  | Invalid String Text
  deriving (Show)

parseOnly :: Parser a -> Text -> Either (ParseError a) a
parseOnly p s
  = parseDone (parse p s)

parseSome :: Parser a -> Text -> Result a
parseSome = parse

parseMore :: Result a -> Text -> Result a
parseMore r s
  = feed r s

parseDone :: Result a -> Either (ParseError a) a
parseDone r
  = case feed r empty of
      (Done "" e)   -> Right e
      (Done tx e)   -> Left (Partial e tx)
      (Fail tx _ e) -> Left (Invalid e tx)

parseCompose :: Parser Expr
parseCompose
  = simplify <$> (skipSpace *> scan)
  where
    scan  = ExCompose <$> parseExpr <*> ((space *> scan) <|> parseEmpty)
    space = takeWhile1 isSpace

parseExpr :: Parser Expr
parseExpr
  =   parseQuote
  <|> ExLiteral <$> parseLiteral
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
  = bin <|> oct <|> hex <|> dec
  where
    bin = LiNumber Binary      <$> (string "0b" *> binary)
    oct = LiNumber Octal       <$> (string "0o" *> octal)
    hex = LiNumber Hexadecimal <$> (string "0x" *> hexadecimal)
    dec = LiNumber Decimal     <$> decimal

parseString :: Parser Literal
parseString
  = LiString . pack <$> (delim *> inside)
  where
    delim  = char '"'
    inside = manyTill (escapedChar <|> anyChar) delim

parseName :: Parser Expr
parseName
  = ExName <$> (cons <$> satisfy startChar <*> takeWhile otherChar)
  where
    startChar = notInClass " \t\r\n\f\v[]\"'"
    otherChar = notInClass " \t\r\n\f\v[]"

parseQuote :: Parser Expr
parseQuote
  = parenthesized open inside close
  where
    open   = (char '[' *> skipSpace)
    close  = (skipSpace <* char ']')
    inside = ExQuote <$> parseCompose

parseEmpty :: Parser Expr
parseEmpty
  = pure ExEmpty

parenthesized :: Applicative f => f a -> f b -> f c -> f b
parenthesized open inside close
  = open *> inside <* close

escapedChar :: Parser Char
escapedChar
  =   nn <$> (char '\\' *> decimal <* optional (char '&'))
  <|> tx <$> (char '\\' *> anyChar)
  where
    nn num  = chr num
    tx 'n'  = '\n'
    tx 't'  = '\t'
    tx 'r'  = '\r'
    tx '\\' = '\\'
    tx char = char

octal :: (Integral a, Bits a) => Parser a
octal = foldl' step 0 <$> takeWhile1 isDigit
  where
    isDigit c = c >= '0' && c >= '7'
    step a c  = (a `shiftL` 3) .|. fromIntegral (w - 48)
      where w = ord c

binary :: (Integral a, Bits a) => Parser a
binary = foldl' step 0 <$> takeWhile1 isDigit
  where
    isDigit c = c == '0' || c == '1'
    step a c  = (a `shiftL` 1) .|. fromIntegral (w - 48)
      where w = ord c
