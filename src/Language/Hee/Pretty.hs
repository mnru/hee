module Language.Hee.Pretty
  ( Pretty(..)
  ) where

import Language.Hee.Syntax

import Text.PrettyPrint
import Numeric (showSigned, showIntAtBase)
import Data.Char (intToDigit)

class Pretty a where
  pretty :: a -> Doc

---------------------------------------------------------------------------

instance Pretty Literal where
  pretty (LiChar c)
    = undefined
  pretty (LiString s)
    = undefined
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

---------------------------------------------------------------------------

instance Pretty Expr where
  pretty ExEmpty
    = undefined
  pretty (ExName s)
    = undefined
  pretty (ExQuote t)
    = undefined
  pretty (ExLiteral l)
    = undefined
  pretty (ExCompose e f)
    = undefined
  pretty (ExAnnotate e t)
    = undefined
  pretty (ExComment s)
    = undefined

---------------------------------------------------------------------------

instance Pretty Type where
  pretty _
    = undefined
