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

type Unifier a
  = a -> a -> Either UnifyError Substitution

reflexive :: Unify a => Unifier a -> a -> Bool
reflexive op x = x `op` x == Right empty

-- symmetric :: Unify a => Unifier a -> a -> a -> Bool
-- symmetric op a b = a `op` b == b `op` a
--
-- assoc :: Unify a => Unifier a -> a -> a -> a -> Bool
-- assoc op a b c  = a `op` (b `op` c) == (a `op` b) `op` c
--
-- apply :: Unify a => Unifier a -> a -> a -> Bool
-- apply op a b = either (const False) (a ==) $ a `op` b
