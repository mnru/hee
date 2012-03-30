{-# LANGUAGE OverloadedStrings #-}

module Hee.Parser
  where

import Prelude hiding (takeWhile)
import Control.Applicative ((<|>), (*>), (<$>))

import Data.Attoparsec
import Data.ByteString (cons)
import Data.Word
import Hee.Terms

heeExpr = undefined
--    <|> heeQuote
--    <|> heeName
--    <|> heeString
--    <|> heeNumber
--    <|> heeChar

heeQuote = do word8 91
              word8 93
              return ()

heeName = do head <- satisfy $ notInClass "[] \t\r\n'\""
             tail <- takeWhile $ notInClass "[] \t\r\n"
             return $ cons head tail

heeNumber = undefined --

heeString = do word8 34
               word8 34
               return ()

heeChar = do string "'"
             escape <|> single
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
