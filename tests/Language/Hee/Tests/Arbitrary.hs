module Language.Hee.Tests.Arbitrary
  ( TmName(..)
  , TmQuote(..)
  , TmLiteral(..)
  , TmCompose(..)
  , TmAnnotation(..)
  , TmComment(..)
  , LiChar(..)
  , LiString(..)
  , LiNumber(..)
  ) where

import Data.Char (isSpace, isPrint, isAlphaNum, isAscii)
import Data.Text (Text, pack, null)
import Control.Applicative
import Test.QuickCheck

import Language.Hee.Terms
import Language.Hee.Types
import Language.Hee.Syntax
import Language.Hee.Simplify

newtype TmName       = RnName       { tmName       :: Term } deriving (Show)
newtype TmQuote      = RnQuote      { tmQuote      :: Term } deriving (Show)
newtype TmLiteral    = RnLiteral    { tmLiteral    :: Term } deriving (Show)
newtype TmCompose    = RnCompose    { tmCompose    :: Term } deriving (Show)
newtype TmAnnotation = RnAnnotation { tmAnnotation :: Term } deriving (Show)
newtype TmComment    = RnComment    { tmComment    :: Term } deriving (Show)

newtype LiChar       = RnChar   { liChar       :: Literal } deriving (Show)
newtype LiString     = RnString { liString     :: Literal } deriving (Show)
newtype LiNumber     = RnNumber { liNumber     :: Literal } deriving (Show)

-----------------------------------------------------------------------------

instance Arbitrary TmName where
  arbitrary
    = RnName . TmName . pack <$> arbitrary `suchThat` valid
    where
      valid ""     = False
      valid s@(x:xs)
        | x `elem` start        = False
        | any (`elem` other) xs = False
        | any isSpace s         = False
        | otherwise             = all (\x -> isPrint x && isAscii x) s
      start = " \t\r\n\f\v[]\"'0123456789+-"
      other = " \t\r\n\f\v[]"

instance Arbitrary TmQuote where
  arbitrary = RnQuote . TmQuote <$> arbitrary `suchThat` valid
    where
      valid (TmCompose _ _) = False
      valid _               = True

instance Arbitrary TmLiteral where
  arbitrary = RnLiteral . TmLiteral <$> arbitrary

instance Arbitrary TmCompose where
  arbitrary = RnCompose . simplify <$> (TmCompose <$> term <*> term)
    where term = arbitrary `suchThat` (/= TmEmpty)

instance Arbitrary TmAnnotation where
  arbitrary = RnAnnotation <$> (TmAnnotation <$> arbitrary <*> arbitrary)

instance Arbitrary TmComment where
  arbitrary = RnComment . TmComment <$> arbitrary

instance Arbitrary LiChar where
  arbitrary = RnChar . LiChar <$> arbitrary

instance Arbitrary LiString where
  arbitrary = RnString . LiString . pack <$> arbitrary `suchThat` valid
    where valid = all (\x -> isAlphaNum x && isAscii x)

instance Arbitrary LiNumber where
  arbitrary = RnNumber . LiNumber <$> arbitrary

instance Arbitrary Type where
  arbitrary = pure Null

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary Term where
  arbitrary
    = frequency [(1, pure TmEmpty)
                ,(4, tmName       <$> arbitrary)
                ,(3, tmQuote      <$> arbitrary)
                ,(2, tmLiteral    <$> arbitrary)
                ,(5, tmCompose    <$> arbitrary)
                ,(0, tmAnnotation <$> arbitrary)
                ,(0, tmComment    <$> arbitrary)]

instance Arbitrary Literal where
  arbitrary
    = frequency [(1, liChar   <$> arbitrary)
                ,(3, liString <$> arbitrary)
                ,(4, liNumber <$> arbitrary)]
