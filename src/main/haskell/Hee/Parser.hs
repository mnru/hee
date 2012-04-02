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
import Data.Attoparsec (Parser)
import Data.Word
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
heeQuote = do lbracket
              A.skipSpace
              expr <- heeExpr
              A.skipSpace
              rbracket
              return $ TmQuote expr
  where lbracket = A.char '['
        rbracket = A.char ']'

heeString :: Parser Term
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

heeChar :: Parser Term
heeChar = do A.char8 '\''
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
heeNameOrNumber = undefined
  -- S '-+'           -> T
  -- S '0'            -> Z
  -- S '123456789'    -> DEC
  -- S '.'            -> FLT
  -- S _              -> ID
  -- Z 'b'            -> BIN
  -- Z 'o'            -> OCT
  -- Z 'x'            -> HEX
  -- Z '0123456789'   -> DEC
  -- Z _              -> ID
  -- DEC '0123456789' -> DEC
  -- DEC '.'          -> FLT
  -- DEC _            -> ID
  -- FLT '0123456789' -> FLT
  -- FLT _            -> ID


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
