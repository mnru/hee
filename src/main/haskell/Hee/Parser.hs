{-# LANGUAGE OverloadedStrings #-}

module Hee.Parser
  where

import Prelude hiding (takeWhile)
import Control.Applicative ((<|>), (*>), (<$>))

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Attoparsec
import Data.ByteString (cons)
import Data.Word
import Data.List (foldl')
import Hee.Terms

heeSpace = inClass " \t\r\n"

heeExpr = do terms <- sepBy heeTerm (satisfy heeSpace)
             return $ foldl' TmCompose TmEmpty terms

heeTerm = heeQuote
      <|> heeName
      <|> heeNumber
      <|> heeChar
      <|> heeString

heeQuote = do word8 91 -- [
              skipWhile heeSpace
              expr <- heeExpr
              skipWhile heeSpace
              word8 93 -- ]
              return $ TmQuote expr

heeName = do head <- satisfy   $ notInClass "[] \t\r\n'\"0123456789"
             tail <- takeWhile $ notInClass "[] \t\r\n"
             return $ TmName $ cons head tail

heeNumber = do whole <- parseNum
               return $ TmLiteral $ LiInt whole
  where parse b = B.foldl' (digit b) 0
        digit b accum n
          | n <= 57   = accum * b + fromIntegral (n - 48) -- 0
          | n <= 90   = accum * b + fromIntegral (n - 55) -- A
          | otherwise = accum * b + fromIntegral (n - 87) -- a

        parseNum = (parse  2 `fmap` radixNum "0b" "01")
               <|> (parse  8 `fmap` radixNum "0o" "01234567")
               <|> (parse 16 `fmap` radixNum "0x" "0123456789abcdefABCDEF")
               <|> (parse 10 `fmap` radixNum ""   "0123456789")
        radixNum p ds = string p >> takeWhile1 (inClass ds)

heeString = do word8 34 -- "
               word8 34 -- "
               return $ TmLiteral $ LiString "x"

heeChar = do word8 39 -- '
             escape <|> single
             return $ TmLiteral $ LiChar 'x'
  where single = anyWord8
        escape = word8 92 >> anyWord8
        tx  92 = 92 -- '\\
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
