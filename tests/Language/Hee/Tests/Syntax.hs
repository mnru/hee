module Language.Hee.Tests.Syntax
  ( tests
  ) where

import Control.Applicative
import Language.Hee.Terms
import Language.Hee.Tests.Arbitrary

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

tests :: [Test]
tests =
  [ testGroup "syntax"
    [ testProperty "char"       $ pShowRead . liChar
    , testProperty "string"     $ pShowRead . liString
    , testProperty "number"     $ pShowRead . liNumber
    , testProperty "empty"      $ pShowRead TmEmpty
    , testProperty "name"       $ pShowRead . tmName
    , testProperty "quote"      $ pShowRead . tmQuote
    , testProperty "literal"    $ pShowRead . tmLiteral
    , testProperty "compose"    $ pShowRead . tmCompose
    -- , testProperty "annotation" $ pShowRead . unE
    -- , testProperty "comment"    $ pShowRead . unF
    ]
  ]

pShowRead :: (Eq a, Show a, Read a) => a -> Bool
pShowRead = (==) <*> (read . show)
