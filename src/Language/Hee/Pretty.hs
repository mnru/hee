module Language.Hee.Pretty
  ( Pretty(..)
  ) where

import Language.Hee.Syntax

import Prelude hiding (foldr)
import Data.Char (ord, isPrint, isAscii, isSpace, intToDigit)
import Data.Text (foldr)
import Numeric (showSigned, showIntAtBase)
import Text.PrettyPrint

class Pretty a where
  pretty :: a -> Doc

---------------------------------------------------------------------------

instance Pretty Literal where
  pretty (LiChar c)
    = text ("'" ++ formatChar c)

  pretty (LiString s)
    = doubleQuotes . text $ foldr ((++) . formatChar) "" s

  pretty (LiNumber Binary n)
    = text "0b" <> text (bin n "")
    where bin = showSigned (showIntAtBase  2 intToDigit) 0

  pretty (LiNumber Octal n)
    = text "0o" <> text (oct n "")
    where oct = showSigned (showIntAtBase  8 intToDigit) 0

  pretty (LiNumber Decimal n)
    = text (dec n "")
    where dec = showSigned (showIntAtBase 10 intToDigit) 0

  pretty (LiNumber Hexadecimal n)
    = text "0x" <> text (hex n "")
    where hex = showSigned (showIntAtBase 16 intToDigit) 0

formatChar :: Char -> String
formatChar '\n' = "\\n"
formatChar '\t' = "\\t"
formatChar '\r' = "\\r"
formatChar '\\' = "\\\\"
formatChar c
  | not $ isPrint c = '\\' : (show $ ord c) ++ ";"
  | not $ isAscii c = '\\' : (show $ ord c) ++ ";"
  | isSpace c       = '\\' : (show $ ord c) ++ ";"
  | otherwise       = c : ""

---------------------------------------------------------------------------

instance Pretty Expr where
  pretty ExEmpty
    = empty

  pretty (ExName s)
    = undefined

  pretty (ExQuote t)
    = brackets (pretty t)

  pretty (ExLiteral l)
    = pretty l

  pretty (ExCompose e f)
    = pretty e <+> pretty f

  pretty (ExAnnotate e t)
    = undefined

  pretty (ExComment s)
    = undefined

---------------------------------------------------------------------------

instance Pretty Type where
  pretty _
    = undefined
