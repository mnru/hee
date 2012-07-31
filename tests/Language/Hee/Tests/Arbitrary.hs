{-# LANGUAGE OverloadedStrings #-}

module Language.Hee.Tests.Arbitrary
  ( ExName(..)
  , ExQuote(..)
  , ExLiteral(..)
  , ExCompose(..)
  , ExAnnotate(..)
  , ExComment(..)
  , LiChar(..)
  , LiString(..)
  , LiNumber(..)
  , SrcNamed(..)
  , SrcEscaped(..)
  , SrcExcaped(..)
  , SrcPlain(..)
  ) where

import Data.Char (ord, isSpace, isPrint, isAlphaNum, isAscii)
import Data.Text (Text, pack, unpack, append, snoc)
import Control.Applicative
import Test.QuickCheck

import Language.Hee.Syntax
import Language.Hee.Simplify

newtype ExName     = RnName     { exName       :: Expr } deriving (Show)
newtype ExQuote    = RnQuote    { exQuote      :: Expr } deriving (Show)
newtype ExLiteral  = RnLiteral  { exLiteral    :: Expr } deriving (Show)
newtype ExCompose  = RnCompose  { exCompose    :: Expr } deriving (Show)
newtype ExAnnotate = RnAnnotate { exAnnotation :: Expr } deriving (Show)
newtype ExComment  = RnComment  { exComment    :: Expr } deriving (Show)

newtype LiChar     = RnChar   { liChar   :: Literal } deriving (Show)
newtype LiString   = RnString { liString :: Literal } deriving (Show)
newtype LiNumber   = RnNumber { liNumber :: Literal } deriving (Show)

newtype SrcNamed   = SrcNamed   { srcNamed   :: Text } deriving (Show)
newtype SrcPlain   = SrcPlain   { srcPlain   :: Text } deriving (Show)
newtype SrcEscaped = SrcEscaped { srcEscaped :: Text } deriving (Show)
newtype SrcExcaped = SrcExcaped { srcExcaped :: Text } deriving (Show)

-----------------------------------------------------------------------------

instance Arbitrary ExName where
  arbitrary
    = RnName . ExName . pack <$> arbitrary `suchThat` valid
    where
      valid ""                  = False
      valid s@(x:xs)
        | x `elem` start        = False
        | any (`elem` other) xs = False
        | any isSpace s         = False
        | otherwise             = all (\x -> isPrint x) s
      start = " \t\r\n\f\v[]\"'0123456789+-"
      other = " \t\r\n\f\v[]"

instance Arbitrary ExQuote where
  arbitrary = RnQuote . ExQuote <$> arbitrary `suchThat` valid
    where
      valid (ExCompose _ _) = False
      valid _               = True

instance Arbitrary ExLiteral where
  arbitrary = RnLiteral . ExLiteral <$> arbitrary

instance Arbitrary ExCompose where
  arbitrary = RnCompose . simplify <$> (ExCompose <$> expr <*> expr)
    where expr = arbitrary `suchThat` (/= ExEmpty)

instance Arbitrary ExAnnotate where
  arbitrary = RnAnnotate <$> (ExAnnotate <$> arbitrary <*> arbitrary)

instance Arbitrary ExComment where
  arbitrary = RnComment . ExComment . pack <$> arbitrary

instance Arbitrary Radix where
  arbitrary = elements [Binary, Octal, Decimal, Hexadecimal]

instance Arbitrary LiChar where
  arbitrary = RnChar . LiChar <$> arbitrary

instance Arbitrary LiString where
  arbitrary = RnString . LiString . pack <$> arbitrary

instance Arbitrary LiNumber where
  arbitrary = RnNumber <$> (LiNumber <$> arbitrary <*> arbitrary)

instance Arbitrary Type where
  arbitrary = pure Null

-- Characters with special escape sequences
instance Arbitrary SrcNamed where
  arbitrary = SrcNamed . format <$> elements ['\'', '\r', '\n', '\t']
    where
      format '\'' = "'\\'"
      format '\\' = "'\\\\"
      format '\r' = "'\\r"
      format '\n' = "'\\n"
      format '\t' = "'\\t"

-- Ordinary characters like 'a, 'b, etc
instance Arbitrary SrcPlain where
  arbitrary = SrcPlain . format <$> arbitrary `suchThat` valid
    where
      format  = snoc "'"
      valid c = not (named c || numbered c)
      named c = c `elem` "\r\n\t'\\"
      numbered c = c < '!' || c > '~'

-- Characters encoded as '\nnn;
instance Arbitrary SrcEscaped where
  arbitrary = SrcEscaped . format <$> arbitrary `suchThat` valid
    where
      format  = append "'\\" . flip (snoc . pack . show . ord) ';'
      valid c = not (named c) && numbered c
      named c = c `elem` "\r\n\t'\\"
      numbered c = c < '!' || c > '~'

-- Characters encoded as '\nnn; that don't need to be
instance Arbitrary SrcExcaped where
  arbitrary = SrcExcaped . format <$> arbitrary `suchThat` valid
    where
      format  = append "'\\" . flip (snoc . pack . show . ord) ';'
      valid c = named c || not (numbered c)
      named c = c `elem` "\r\n\t'\\"
      numbered c = c < '!' || c > '~'

instance Arbitrary Expr where
  arbitrary
    = frequency [(1, pure ExEmpty)
                ,(4, exName       <$> arbitrary)
                ,(3, exQuote      <$> arbitrary)
                ,(2, exLiteral    <$> arbitrary)
                ,(5, exCompose    <$> arbitrary)
                ,(0, exAnnotation <$> arbitrary)
                ,(0, exComment    <$> arbitrary)]

instance Arbitrary Literal where
  arbitrary
    = frequency [(1, liChar   <$> arbitrary)
                ,(3, liString <$> arbitrary)
                ,(4, liNumber <$> arbitrary)]
