module Language.Hee.Tests.Syntax
  ( tests
  ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Applicative
import Data.Text (Text)
import Language.Hee.Tests.Arbitrary
import Language.Hee.Syntax
import Language.Hee.Pretty
import Language.Hee.Parser

tests :: [Test]
tests =
  [ testGroup "syntax"
    [ testProperty "char"     $ pShowRead . liChar
    , testProperty "string"   $ pShowRead . liString
    , testProperty "number"   $ pShowRead . liNumber
    , testProperty "empty"    $ pShowRead ExEmpty
    , testProperty "name"     $ pShowRead . exName
    , testProperty "quote"    $ pShowRead . exQuote
    , testProperty "literal"  $ pShowRead . exLiteral
    , testProperty "compose"  $ pShowRead . exCompose
    ]
  ]

pShowRead :: (Parsable a, Pretty a, Eq a) => a -> Bool
pShowRead
  = (eq . Right) <*> showRead
  where
    showRead = (parseOnly parser) . renderText . pretty
    eq (Right a) (Right b) = a == b
    eq _ _                 = False
