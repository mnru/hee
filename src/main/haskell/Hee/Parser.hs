{-# LANGUAGE OverloadedStrings #-}

module Hee.Parser
  ( heeExpr )
  where

import Prelude hiding (takeWhile)
import Control.Applicative ((<|>), (*>), (<$>))

import qualified Data.Attoparsec.Char8 as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import qualified Data.Attoparsec as I
--import Data.Attoparsec
import Data.Word
import Data.List (foldl')
import Hee.Terms

heeExpr = do terms <- A.sepBy heeTerm (A.many1 A.space)
             return $ foldl' TmCompose TmEmpty terms

heeTerm = heeQuote
      <|> heeNumber
      <|> heeName
      <|> heeChar
      <|> heeString

heeQuote = do lbracket
              A.skipSpace
              expr <- heeExpr
              A.skipSpace
              rbracket
              return $ TmQuote expr
  where lbracket = A.char8 '['
        rbracket = A.char8 ']'

heeName = do head <- A.satisfy   $ A.notInClass "[] \t\r\n'\""
             tail <- A.takeWhile $ A.notInClass "[] \t\r\n"
             return $ TmName $ B.cons head tail

heeNumber = do sign  <- A.option id $ A.choice [plus, minus]
               whole <- parseNum
               return $ TmLiteral $ LiInt $ sign whole
  where plus  = A.char8 '+' >> return id
        minus = A.char8 '-' >> return negate

        parse b = BS.foldl' (digit b) 0
        digit b accum n
          | n <= 57   = accum * b + fromIntegral (n - 48) -- 0
          | n <= 90   = accum * b + fromIntegral (n - 55) -- A
          | otherwise = accum * b + fromIntegral (n - 87) -- a

        radixNum p ds = A.string p >> A.takeWhile1 (A.inClass ds)
        parseNum = parse  2 `fmap` radixNum "0b" "01"
               <|> parse  8 `fmap` radixNum "0o" "01234567"
               <|> parse 16 `fmap` radixNum "0x" "0123456789abcdefABCDEF"
               <|> parse 10 `fmap` radixNum ""   "0123456789"

heeString = do dquote
               chars <- A.manyTill (escape <|> single) dquote
               return $ TmLiteral $ LiString $ BS.pack chars
  where dquote = A.char8 '"'
        single = I.anyWord8
        escape = A.char8 '\\' >> tx `fmap` I.anyWord8
        tx 110 = 10 -- '\n
        tx 114 = 13 -- '\r
        tx 116 = 9  -- '\t
        tx   c = c

heeChar = do squote
             char <- escape <|> single
             return $ TmLiteral $ LiChar char
  where squote = A.char8 '\''
        single = I.anyWord8
        escape = A.char8 '\\' >> tx `fmap` I.anyWord8
        tx 110 = 10 -- '\n
        tx 114 = 13 -- '\r
        tx 116 = 9  -- '\t
        tx   c = c

-- Data.Attoparsec.ByteString
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
--   I.try
--   I.<?>
--   I.parseOnly
--   I.word8
--   I.anyWord8
--   I.notWord8
--   I.inClass
--   I.notInClass
--   I.storable
--   I.satisfy
--   I.satisfyWith
--   I.skip
--   I.skipWhile
--   I.take
--   I.scan
--   I.string
--   I.stringTransform
--   I.takeWhile
--   I.takeWhile1
--   I.takeTill
--   I.takeByteString
--   I.takeLazyByteString
--   I.endOfLine
--   I.endOfInput
--   I.atEnd
--
--   parse
--   feed
--   parseWith
--   parseTest
--   maybeResult
--   eitherResult
