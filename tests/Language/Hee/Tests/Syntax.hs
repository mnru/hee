module Language.Hee.Tests.Syntax
  ( tests
  ) where

import Control.Applicative
import Language.Hee.Syntax
import Language.Hee.Tests.Arbitrary

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

tests :: [Test]
tests = []
--  [ testGroup "syntax"
--    [ testProperty "char"     $ pShowRead . liChar
--    , testProperty "string"   $ pShowRead . liString
--    , testProperty "number"   $ pShowRead . liNumber
--    , testProperty "empty"    $ pShowRead ExEmpty
--    , testProperty "name"     $ pShowRead . exName
--    , testProperty "quote"    $ pShowRead . exQuote
--    , testProperty "literal"  $ pShowRead . exLiteral
--    , testProperty "compose"  $ pShowRead . exCompose
--    -- , testProperty "annotate" $ pShowRead . exAnnotate
--    -- , testProperty "comment"  $ pShowRead . exComment
--    ]
--  ]
--
--pShowRead :: (Eq a, Show a, Read a) => a -> Bool
--pShowRead = (==) <*> (read . show)
