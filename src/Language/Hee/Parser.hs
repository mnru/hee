{-# LANGUAGE OverloadedStrings #-}

module Language.Hee.Parser
  ( Parsable(..)
  , ParseError(..)
  , parseOnly
  , parseSome
  , parseMore
  , parseDone
  , parseExpr     -- Parser Expression
  , parseLiteral  -- Parser Literal
  , pruneExpr
  ) where

import Language.Hee.Syntax

import Prelude hiding (takeWhile)
import Control.Applicative hiding (empty)
import Data.Bits (Bits, shiftL, (.|.))
import Data.Char (isSpace, isOctDigit, chr, ord)
import Data.Text (Text, cons, pack, empty, foldl')
import Data.Attoparsec.Text hiding (parseOnly, Partial)

data ParseError a
  = Partial a Text
  | Invalid String Text
  deriving (Show)

instance (Eq a) => Eq (ParseError a) where
  (Partial a t) == (Partial a' t') = a == a' && t == t'
  (Invalid s t) == (Invalid s' t') = s == s' && t == t'
  _             == _               = False

class Parsable a where
  parser :: Parser a

instance Parsable Literal where
  parser = parseLiteral

instance Parsable Expression where
  parser = parseExpr

---------------------------------------------------------------------------

pruneExpr :: Expression -> Expression
pruneExpr (ECompose (ECompose a b) c) = pruneExpr $ ECompose a (ECompose b c)
pruneExpr (EQuote a)                  = EQuote $ pruneExpr a
pruneExpr (EAnnotate e t)             = EAnnotate (pruneExpr e) t
pruneExpr (ECompose a b)              = case (pruneExpr a, pruneExpr b) of
                                         (EEmpty, b') -> b'
                                         (a', EEmpty) -> a'
                                         (a', b')      -> ECompose a' b'
pruneExpr e                           = e

parseOnly :: Parser a -> Text -> Either (ParseError a) a
parseOnly p s
  = parseDone (parse p s)

parseSome :: Parser a -> Text -> Result a
parseSome = parse

parseMore :: Result a -> Text -> Result a
parseMore
  = feed

parseDone :: Result a -> Either (ParseError a) a
parseDone r
  = case feed r empty of
      (Done "" e)   -> Right e
      (Done tx e)   -> Left (Partial e tx)
      (Fail tx _ e) -> Left (Invalid e tx)

---------------------------------------------------------------------------

parseExpr :: Parser Expression
parseExpr
  = pruneExpr <$> (skipSpace *> scan)
  where
    scan  = ECompose <$> expr <*> ((space *> scan) <|> parseEmpty)
    space = takeWhile1 isSpace
    expr  = parseQuote
        <|> ELiteral <$> parseLiteral
        <|> parseName
        <|> parseEmpty

parseLiteral :: Parser Literal
parseLiteral
  =   parseChar
  <|> parseString
  <|> parseNumber
  <|> parseBool

parseBool :: Parser Literal
parseBool
  = LBool <$> (true <|> false)
  where
    true  = string "true"  *> pure True
    false = string "false" *> pure False

parseChar :: Parser Literal
parseChar
  = LChar <$> (delim *> (escapedChar <|> anyChar))
  where
    delim = char '\''

parseNumber :: Parser Literal
parseNumber
  = bin <|> oct <|> hex <|> dec
  where
    bin = LNumber Binary      <$> signed (string "0b" *> binary)
    oct = LNumber Octal       <$> signed (string "0o" *> octal)
    hex = LNumber Hexadecimal <$> signed (string "0x" *> hexadecimal)
    dec = LNumber Decimal     <$> signed decimal

parseString :: Parser Literal
parseString
  = LString . pack <$> (delim *> inside)
  where
    delim  = char '"'
    inside = manyTill (escapedChar <|> anyChar) delim

parseName :: Parser Expression
parseName
  = EName <$> (cons <$> satisfy startChar <*> takeWhile otherChar)
  where
    startChar = notInClass " \t\r\n\f\v[]\"'"
    otherChar = notInClass " \t\r\n\f\v[]"

parseQuote :: Parser Expression
parseQuote
  = parenthesized open inside close
  where
    open   = char '[' *> skipSpace
    close  = skipSpace <* char ']'
    inside = EQuote <$> parseExpr

parseEmpty :: Parser Expression
parseEmpty
  = pure EEmpty

escapedChar :: Parser Char
escapedChar
  = char '\\' *> ((number <* char ';') <|> named)
  where
    number = chr <$> digits
    digits = (string "0b" *> binary)
         <|> (string "0o" *> octal)
         <|> (string "0x" *> hexadecimal)
         <|> decimal
    named  = char 'r' *> pure '\r'
         <|> char 'n' *> pure '\n'
         <|> char 't' *> pure '\t'
         <|> char '\\'
         <|> char '\''
         <|> char '"'

---------------------------------------------------------------------------

parenthesized :: Applicative f => f a -> f b -> f c -> f b
parenthesized open inside close
  = open *> inside <* close

octal :: (Integral a, Bits a) => Parser a
octal = foldl' step 0 <$> takeWhile1 isOctDigit
  where
    step a c  = (a `shiftL` 3) .|. fromIntegral (w - 48)
      where w = ord c

binary :: (Integral a, Bits a) => Parser a
binary = foldl' step 0 <$> takeWhile1 isDigit
  where
    isDigit c = c == '0' || c == '1'
    step a c  = (a `shiftL` 1) .|. fromIntegral (w - 48)
      where w = ord c
