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
  [ testGroup "ast -> string -> ast"
    [ testProperty "char"     $ reparse . liChar
    , testProperty "string"   $ reparse . liString
    , testProperty "number"   $ reparse . liNumber
    , testProperty "empty"    $ reparse ExEmpty
    , testProperty "name"     $ reparse . exName
    , testProperty "quote"    $ reparse . exQuote
    , testProperty "literal"  $ reparse . exLiteral
    , testProperty "compose"  $ reparse . exCompose
  ],
  testGroup "string -> ast -> string"
    [ testProperty "char named"   $ reprint (parser :: Parser Literal) . srcNamed
    , testProperty "char plain"   $ reprint (parser :: Parser Literal) . srcPlain
    , testProperty "char escaped" $ reprint (parser :: Parser Literal) . srcEscaped
    , testProperty "char escaped" $ not . reprint (parser :: Parser Literal) . srcExcaped
    , testProperty "string"       $ reprint (parser :: Parser Literal) . srcString
    ]
  ]

-- True when parse . pretty == id
reparse :: (Parsable a, Pretty a, Eq a) => a -> Bool
reparse
  = ((==) . Right) <*> reparse
  where
    reparse = (parseOnly parser) . renderText . pretty

-- True when pretty . parse == id
reprint :: (Parsable a, Pretty a, Eq a) => Parser a -> Text -> Bool
reprint p s
  = case parseOnly p s of
      Left _  -> False
      Right e -> s == renderText (pretty e)
