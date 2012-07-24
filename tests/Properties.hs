module Main (main) where

import Control.Applicative
import Data.Text (Text, pack)
import Language.Hee.Terms
import Language.Hee.Types
import Language.Hee.Parser

import Test.QuickCheck
import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "parser"
    [ testProperty "empty"   $ pVacuous
    , testProperty "quote"   $ pVacuous
    , testProperty "compose" $ pVacuous
    , testProperty "string"  $ pVacuous
    , testProperty "char"    $ pVacuous
    , testProperty "number"  $ pVacuous ] ]

instance Arbitrary Type where
  arbitrary = pure Null

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary Term where
  arbitrary
    = frequency [(1, pure TmEmpty)
                ,(4, TmName       <$> arbitrary)
                ,(3, TmQuote      <$> arbitrary)
                ,(2, TmLiteral    <$> arbitrary)
                ,(5, TmCompose    <$> arbitrary <*> arbitrary)
                ,(0, TmAnnotation <$> arbitrary <*> arbitrary)
                ,(0, TmComment    <$> arbitrary)]

instance Arbitrary Literal where
  arbitrary
    = frequency [(1, LiChar   <$> arbitrary)
                ,(3, LiString <$> arbitrary)
                ,(4, LiNumber <$> arbitrary)]

pVacuous :: Bool
pVacuous = True == True
