module Language.Hee.Pretty
  ( Pretty(..)
  , renderText
  , renderString
  ) where

import Language.Hee.Syntax

import Prelude hiding (foldr)
import Control.Applicative hiding (empty)
import Data.Char (ord, isPrint, isAscii, isSpace, intToDigit)
import Data.Text (Text, foldr, pack, unpack)
import Numeric (showIntAtBase)
import Text.PrettyPrint

renderText :: Pretty a => a -> Text
renderText = pack . render . pretty

renderString :: Pretty a => a -> String
renderString = render . pretty

class Pretty a where
  pretty :: a -> Doc

---------------------------------------------------------------------------

instance Pretty Literal where
  pretty (LChar c)
    = text ('\'' : formatChar c)
    where
      formatChar '\n' = "\\n"
      formatChar '\t' = "\\t"
      formatChar '\r' = "\\r"
      formatChar '\\' = "\\\\"
      formatChar c
        | not $ isPrint c = '\\' : show (ord c) ++ ";"
        | not $ isAscii c = '\\' : show (ord c) ++ ";"
        | isSpace c       = '\\' : show (ord c) ++ ";"
        | otherwise       = c : ""

  pretty (LString s)
    = doubleQuotes . text $ foldr ((++) . formatChar) "" s
    where
      formatChar '\n' = "\\n"
      formatChar '\t' = "\\t"
      formatChar '\r' = "\\r"
      formatChar '\\' = "\\\\"
      formatChar '"'  = "\\\""
      formatChar c
        | not $ isPrint c = '\\' : show (ord c) ++ ";"
        | not $ isAscii c = '\\' : show (ord c) ++ ";"
        | isSpace c       = '\\' : show (ord c) ++ ";"
        | otherwise       = c : ""

  pretty (LNumber radix n)
    = text (format radix n "")
    where
      format :: Radix -> Int -> ShowS
      format Binary      = (.) <$> prefix "0b" <*> digits 2
      format Octal       = (.) <$> prefix "0o" <*> digits 8
      format Decimal     = (.) <$> prefix ""   <*> digits 10
      format Hexadecimal = (.) <$> prefix "0x" <*> digits 16

      digits :: Int -> Int -> ShowS
      digits rad = showIntAtBase rad intToDigit . abs

      prefix :: String -> Int -> ShowS
      prefix pre n
        | n < 0     = showString "-" . showString pre
        | otherwise = showString pre

---------------------------------------------------------------------------

instance Pretty Expression where
  pretty EEmpty
    = empty

  pretty (EName s)
    = text $ unpack s

  pretty (EQuote t)
    = brackets (pretty t)

  pretty (ELiteral l)
    = pretty l

  pretty (ECompose e f)
    = pretty e <+> pretty f

  pretty (EAnnotate _ _)
    = undefined

  pretty (EComment _)
    = undefined

---------------------------------------------------------------------------

instance Pretty Type where
  pretty _
    = undefined
