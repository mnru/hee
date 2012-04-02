{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Hee.Parser
  where

import Prelude hiding (takeWhile)
import Control.Applicative ((<|>), (*>), (<$>))

import qualified Data.Attoparsec.Char8 as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import qualified Data.Attoparsec as I
import Data.Attoparsec (Parser)
import Data.Word
import Data.Char (ord)
import Data.List (foldl')
import Hee.Terms

heeExpr :: Parser Term
heeExpr = do terms <- A.sepBy heeTerm (A.many1 A.space)
             return $ foldl' TmCompose TmEmpty terms

heeTerm :: Parser Term
heeTerm = heeQuote    -- [
      <|> heeChar     -- '
      <|> heeString   -- "
      <|> heeNameOrNumber

heeQuote :: Parser Term
heeQuote =
  do lbracket
     A.skipSpace
     expr <- heeExpr
     A.skipSpace
     rbracket
     return $ TmQuote expr
  where lbracket = A.char '['
        rbracket = A.char ']'

heeString :: Parser Term
heeString =
  do dquote
     chars <- A.manyTill (escape <|> single) dquote
     return $ TmLiteral $ LiString $ BS.pack chars
  where dquote = A.char8 '"'
        single = I.anyWord8
        escape = A.char8 '\\' >> tx `fmap` I.anyWord8
        tx 110 = 10 -- '\n
        tx 114 = 13 -- '\r
        tx 116 = 9  -- '\t
        tx   c = c

heeChar :: Parser Term
heeChar =
  do A.char8 '\''
     char <- escape <|> single
     return $ TmLiteral $ LiChar char
  where squote = A.char8 '\''
        single = I.anyWord8
        escape = A.char8 '\\' >> tx `fmap` I.anyWord8
        tx 110 = 10 -- '\n
        tx 114 = 13 -- '\r
        tx 116 = 9  -- '\t
        tx   c = c

heeNameOrNumber :: Parser Term
heeNameOrNumber =
  do head  <- I.satisfy   $ I.notInClass " \t\r\n\f\v[]\"'"
     tail  <- I.takeWhile $ I.notInClass " \t\r\n\f\v[]"
     return $ heeHar $ B.unpack $ BS.cons head tail

heeHar token =
  case token of
    '+':xs -> positive xs
    '-':xs -> negative xs
    '0':xs -> positive token
    '.':xs -> fract "0123456789" 10 id 0 xs
    _      -> whole "0123456789" 10 id 0 token

  where
    positive = number id
    negative = number negate

    number :: (forall a. Num a => a -> a) -> [Char] -> Term
    number f []           = TmName token
    number f ('0':'b':xs) = whole "01"               2  f 0 xs
    number f ('0':'o':xs) = whole "01234567"         8  f 0 xs
    number f ('0':'x':xs) = whole "0123456789ABCDEF" 16 f 0 xs
    number f ('0':xs)     = whole "0123456789"       10 f 0 xs
    number f ('.':xs)     = fract "0123456789"       10 f 0 xs
    number f s@(x:xs)
      | A.isDigit x       = whole "0123456789" 10 f 0 s
      | otherwise         = TmName token

    digit x
      | x >= 'A' = fromIntegral $ ord x - 55
      | x >= '0' = fromIntegral $ ord x - 48

    whole ::
      [Char]
      -> Int
      -> (forall a. Num a => a -> a)
      -> Int
      -> [Char]
      -> Term
    whole ds b f = loop
      where loop n []       = TmLiteral $ LiInt $ f n
            loop n ('.':xs) = fract ds (fromIntegral b) (f . (+ (fromIntegral n))) 0 (reverse xs)
            loop n (x:xs)
              | x `elem` ds = loop (n*b + digit x) xs
              | otherwise   = TmName token

    fract ::
      [Char]
      -> Float
      -> (forall a. Num a => a -> a)
      -> Float
      -> [Char]
      -> Term
    fract ds b f = loop
      where loop n []       = TmLiteral $ LiFloat $ f (n/b)
            loop n (x:xs)
              | x `elem` ds = loop (n/b + digit x) xs
              | otherwise   = TmName token


-- Notes
--
-- Data.Attoparsec.ByteString
--   parse
--   feed
--   parseWith
--   parseTest
--   maybeResult
--   eitherResult
--
--   Data.Attoparsec.Internal.Types
--   T.Fail
--   T.Done
--   T.Partial
--
--   Data.Attoparsec.Combinator
--   choice
--   count
--   option
--   many1
--   manyTill
--   sepBy
--   sepBy1
--   skipMany
--   skipMany1
--   eitherP
--
--   Data.Attoparsec.ByteString.Internal
--   try
--   <?>
--   parseOnly
--   word8
--   anyWord8
--   notWord8
--   inClass
--   notInClass
--   storable
--   satisfy
--   satisfyWith
--   skip
--   skipWhile
--   take
--   scan
--   string
--   stringTransform
--   takeWhile
--   takeWhile1
--   takeTill
--   takeByteString
--   takeLazyByteString
--   endOfLine
--   endOfInput
--   atEnd
