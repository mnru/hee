module Language.Hee.Tests.Syntax
  ( tests
  ) where

import Test.HUnit
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Applicative
import Data.Char (ord)
import Data.Text (Text, pack)
import Data.Attoparsec.Text (Parser)

import Language.Hee.Tests.Arbitrary
import Language.Hee.Syntax
import Language.Hee.Pretty
import Language.Hee.Parser

tests =
  [ testGroup "syntax"
    [ testProperty "char"     $ readShow . liChar
    , testProperty "string"   $ readShow . liString
    , testProperty "number"   $ readShow . liNumber
    , testProperty "empty"    $ readShow ExEmpty
    , testProperty "name"     $ readShow . exName
    , testProperty "quote"    $ readShow . exQuote
    , testProperty "literal"  $ readShow . exLiteral
    , testProperty "compose"  $ readShow . exCompose
    , testCase "plain chars"    testPlainChars
    , testCase "escaped chars"  testEscapedChars
    ]
  ]

-- True when parse . pretty == id
readShow :: (Parsable a, Pretty a, Eq a) => a -> Bool
readShow
  = ((==) . Right) <*> reparse
  where
    reparse = (parseOnly parser) . renderText . pretty

-- True when pretty . parse == id
showRead :: (Parsable a, Pretty a, Eq a) => Parser a -> Text -> Bool
showRead p s
  = case parseOnly p s of
      Left _  -> False
      Right e -> s == renderText (pretty e)

-- Characters that don't need to be escaped
plain :: String
plain = "0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz"

escape, encode :: Char -> Text
escape c = pack $ "'\\" ++ show (ord c) ++ ";"
encode c = pack $ '\'' : c : ""

testPlainChars =
  filter (showRead (parser :: Parser Literal)) letters @?= letters
  where
    letters = map encode plain

testEscapedChars =
  filter (showRead (parser :: Parser Literal)) letters @?= []
  where
    letters = map escape plain
