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

import Prelude hiding (takeWhile, length)
import Control.Applicative hiding (empty)
import Data.Bits (Bits, shiftL, (.|.))
import Data.Char (isOctDigit, isDigit, chr, ord)
import Data.Text (Text, cons, pack, empty, foldl', length)
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
    scan  = ECompose <$> expr <*> (exprSpace *> scan <|> parseEmpty)
    expr  = parseQuote
        <|> ELiteral <$> parseLiteral
        <|> parseName
        <|> parseEmpty

-- Whitespace such that the next term does not begin flush with a new line
exprSpace :: Parser ()
exprSpace
  =              vSpace *> exprSpace
  <|> takeWhile1 hSpace *> exprSpace'
  where
    vSpace     = string "\n" <|> string "\r\n"
    hSpace c   = c == ' ' || c == '\t'
    exprSpace' = exprSpace <|> pure ()

parseLiteral :: Parser Literal
parseLiteral
  =   parseChar
  <|> parseString
  <|> parseFloat
  <|> parseInteger
  <|> parseBool

parseBool :: Parser Literal
parseBool
  = LBool <$> (true <|> false)
  where
    true  = "true"  .*> pure True
    false = "false" .*> pure False

parseChar :: Parser Literal
parseChar
  = LChar <$> (delim *> (escapedChar <|> anyChar))
  where
    delim = char '\''

parseFloat :: Parser Literal
parseFloat
  = LFloat <$> (build <$> integer <*> fraction <*> exponent)
  where
    integer :: Parser Int
    integer     = signed decimal
    fraction    = parse <$> (char '.' *> takeWhile isDigit)
    parse xs    = case parseOnly number xs of
                    Right n -> fromRational (toRational n) / 10 ^^ (length xs)
                    _       -> 0
    exponent    = ((char 'e' <|> char 'E') *> integer) <|> pure 0
    build a b c = (fromIntegral a + b) * 10 ^^ c

parseInteger :: Parser Literal
parseInteger
  = bin <|> oct <|> hex <|> dec
  where
    bin = LInteger Binary      <$> signed ("0b" .*> binary)
    oct = LInteger Octal       <$> signed ("0o" .*> octal)
    hex = LInteger Hexadecimal <$> signed ("0x" .*> hexadecimal)
    dec = LInteger Decimal     <$> signed decimal

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
    digits = "0b" .*> binary
         <|> "0o" .*> octal
         <|> "0x" .*> hexadecimal
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
    step a c  = (a `shiftL` 3) .|. fromIntegral (ord c - 48)

binary :: (Integral a, Bits a) => Parser a
binary = foldl' step 0 <$> takeWhile1 isDigit
  where
    isDigit c = c == '0' || c == '1'
    step a c  = (a `shiftL` 1) .|. fromIntegral (ord c - 48)
