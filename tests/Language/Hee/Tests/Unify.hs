module Language.Hee.Tests.Unify
  ( tests
  ) where

import Test.HUnit
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Applicative hiding (empty)
import Data.Attoparsec.Text (Parser)

import Language.Hee.Tests.Arbitrary
import Language.Hee.Unify
import Language.Hee.Syntax
import Language.Hee.Substitute

tests =
  [ testGroup "unify"
    [ testProperty "reflexive" $ (reflexive unify :: Stack -> Bool)
    , testProperty "reflexive" $ (reflexive unify :: Type  -> Bool)
    ]
  , testGroup "match"
    [ testProperty "reflexive" $ (reflexive match :: Stack -> Bool)
    , testProperty "reflexive" $ (reflexive match :: Type  -> Bool)
    ]
  ]

reflexive :: Unify a => (a -> a -> Either UnifyError Substitution) -> a -> Bool
reflexive op x = x `op` x == Right empty
