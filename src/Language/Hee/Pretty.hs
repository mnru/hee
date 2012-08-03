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
import Numeric (showSigned, showIntAtBase)
import Text.PrettyPrint

renderText :: Doc -> Text
renderText = pack . render

renderString :: Doc -> String
renderString = render

class Pretty a where
  pretty :: a -> Doc

---------------------------------------------------------------------------

instance Pretty Literal where
  pretty (LiChar c)
    = text ("'" ++ formatChar c)
    where
      formatChar '\n' = "\\n"
      formatChar '\t' = "\\t"
      formatChar '\r' = "\\r"
      formatChar '\\' = "\\\\"
      formatChar c
        | not $ isPrint c = '\\' : (show $ ord c) ++ ";"
        | not $ isAscii c = '\\' : (show $ ord c) ++ ";"
        | isSpace c       = '\\' : (show $ ord c) ++ ";"
        | otherwise       = c : ""

  pretty (LiString s)
    = doubleQuotes . text $ foldr ((++) . formatChar) "" s
    where
      formatChar '\n' = "\\n"
      formatChar '\t' = "\\t"
      formatChar '\r' = "\\r"
      formatChar '\\' = "\\\\"
      formatChar '"'  = "\\\""
      formatChar c
        | not $ isPrint c = '\\' : (show $ ord c) ++ ";"
        | not $ isAscii c = '\\' : (show $ ord c) ++ ";"
        | isSpace c       = '\\' : (show $ ord c) ++ ";"
        | otherwise       = c : ""

  pretty (LiNumber radix n)
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

instance Pretty Expr where
  pretty ExEmpty
    = empty

  pretty (ExName s)
    = text $ unpack s

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
