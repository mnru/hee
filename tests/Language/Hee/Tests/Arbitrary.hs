{-# LANGUAGE OverloadedStrings #-}

module Language.Hee.Tests.Arbitrary
  ( EName(..)
  , EQuote(..)
  , ELiteral(..)
  , ECompose(..)
  , EAnnotate(..)
  , EComment(..)
  , LChar(..)
  , LString(..)
  , LNumber(..)
  , SrcNamed(..)
  , SrcEscaped(..)
  , SrcExcaped(..)
  , SrcString(..)
  , SrcPlain(..)
  ) where

import Data.Char (ord, isSpace, isPrint, isAlphaNum, isAscii)
import Data.Text (Text, pack, unpack, append, snoc, cons)
import Control.Applicative
import Test.QuickCheck

import Language.Hee.Syntax
import Language.Hee.Simplify

-- Expressions
---------------------------------------------------------------------------

newtype EName     = RnName     { eName       :: Expression } deriving (Show)
newtype EQuote    = RnQuote    { eQuote      :: Expression } deriving (Show)
newtype ELiteral  = RnLiteral  { eLiteral    :: Expression } deriving (Show)
newtype ECompose  = RnCompose  { eCompose    :: Expression } deriving (Show)
newtype EAnnotate = RnAnnotate { eAnnotation :: Expression } deriving (Show)
newtype EComment  = RnComment  { eComment    :: Expression } deriving (Show)

instance Arbitrary Expression where
  arbitrary
    = frequency [(1, pure EEmpty)
                ,(4, eName       <$> arbitrary)
                ,(3, eQuote      <$> arbitrary)
                ,(2, eLiteral    <$> arbitrary)
                ,(5, eCompose    <$> arbitrary)
                ,(0, eAnnotation <$> arbitrary)
                ,(0, eComment    <$> arbitrary)]

instance Arbitrary EName where
  arbitrary
    = RnName . EName . pack <$> arbitrary `suchThat` valid
    where
      valid ""                  = False
      valid s@(x:xs)
        | x `elem` start        = False
        | any (`elem` other) xs = False
        | any isSpace s         = False
        | otherwise             = all (\x -> isPrint x) s
      start = " \t\r\n\f\v[]\"'0123456789+-"
      other = " \t\r\n\f\v[]"

instance Arbitrary EQuote where
  arbitrary = RnQuote . EQuote <$> arbitrary `suchThat` valid
    where
      valid (ECompose _ _) = False
      valid _              = True

instance Arbitrary ELiteral where
  arbitrary = RnLiteral . ELiteral <$> arbitrary

instance Arbitrary ECompose where
  arbitrary = RnCompose . simplify <$> (ECompose <$> expr <*> expr)
    where expr = arbitrary `suchThat` (/= EEmpty)

instance Arbitrary EAnnotate where
  arbitrary = RnAnnotate <$> (EAnnotate <$> arbitrary <*> arbitrary)

instance Arbitrary EComment where
  arbitrary = RnComment . EComment . pack <$> arbitrary

-- Literals
---------------------------------------------------------------------------

newtype LChar     = RnChar   { lChar   :: Literal } deriving (Show)
newtype LString   = RnString { lString :: Literal } deriving (Show)
newtype LNumber   = RnNumber { lNumber :: Literal } deriving (Show)

instance Arbitrary Literal where
  arbitrary
    = frequency [(1, lChar   <$> arbitrary)
                ,(3, lString <$> arbitrary)
                ,(3, lNumber <$> arbitrary)]

instance Arbitrary Radix where
  arbitrary = elements [Binary, Octal, Decimal, Hexadecimal]

instance Arbitrary LChar where
  arbitrary = RnChar . LChar <$> arbitrary

instance Arbitrary LString where
  arbitrary = RnString . LString . pack <$> arbitrary

instance Arbitrary LNumber where
  arbitrary = RnNumber <$> (LNumber <$> arbitrary <*> arbitrary)

-- Kinds
---------------------------------------------------------------------------

instance Arbitrary Kind where
  arbitrary
    = frequency [(1, pure KStack)
                ,(2, pure KType)
                ,(1, KConstructor <$> arbitrary <*> arbitrary)]

-- Stacks
---------------------------------------------------------------------------

newtype SPush = RnPush { sPush :: Stack } deriving (Show)
newtype STail = RnTail { sTail :: Stack } deriving (Show)

instance Arbitrary SPush where
  arbitrary = RnPush <$> (SPush <$> arbitrary <*> arbitrary)

instance Arbitrary STail where
  arbitrary = RnTail . STail <$> arbitrary

instance Arbitrary Stack where
  arbitrary
    = frequency [(1, pure SEmpty)
                ,(3, sPush <$> arbitrary)
                ,(4, sTail <$> arbitrary)]

-- Types
---------------------------------------------------------------------------

newtype TStack        = RnStack       { tStack       :: Type } deriving (Show)
newtype TConstructor  = RnConstructor { tConstructor :: Type } deriving (Show)
newtype TApplication  = RnApplication { tApplication :: Type } deriving (Show)
newtype TForall       = RnForall      { tForall      :: Type } deriving (Show)
newtype TQualified    = RnQualified   { tQualified   :: Type } deriving (Show)
newtype TVariable     = RnVariable    { tVariable    :: Type } deriving (Show)

instance Arbitrary TStack where
  arbitrary = RnStack . TStack <$> arbitrary

instance Arbitrary TConstructor where
  arbitrary = RnConstructor <$> (TConstructor . pack <$> arbitrary `suchThat` valid <*> arbitrary)
    where valid = all isAlphaNum

instance Arbitrary TApplication where
  arbitrary
    = do f <- arbitrary `suchThat` (constructor . kind)
         x <- arbitrary `suchThat` ((==) (kind $ domain f) . kind)
         return . RnApplication $ TApplication f x
    where
      constructor (KConstructor _ _) = True
      constructor _                  = False
      domain x = let (KConstructor k _) = kind k in k

--instance Arbitrary TForall where
--  arbitrary = RnForall <$> (TForall <$> arbitrary <*> arbitrary <*> arbitrary)

--instance Arbitrary TQualified where
--  arbitrary = RnQualified <$> (TQualified <$> arbitrary <*> arbitrary)

instance Arbitrary TVariable where
  arbitrary = RnVariable . TVariable <$> arbitrary

instance Arbitrary Type where
  arbitrary
    = frequency [(1, tStack       <$> arbitrary)
                --,(1, tConstructor <$> arbitrary)
                --,(1, tApplication <$> arbitrary)
                --,(0, tForall      <$> arbitrary)
                --,(0, tQualified   <$> arbitrary)
                ,(1, tVariable   <$> arbitrary)]

instance Arbitrary Variable where
  arbitrary = Variable <$> arbitrary <*> arbitrary

instance Arbitrary Bound where
  arbitrary = elements [Rigid, Flexible, Bottom]

-- Source
---------------------------------------------------------------------------

newtype SrcNamed   = SrcNamed   { srcNamed   :: Text } deriving (Show)
newtype SrcPlain   = SrcPlain   { srcPlain   :: Text } deriving (Show)
newtype SrcEscaped = SrcEscaped { srcEscaped :: Text } deriving (Show)
newtype SrcExcaped = SrcExcaped { srcExcaped :: Text } deriving (Show)
newtype SrcString  = SrcString  { srcString  :: Text } deriving (Show)

-- Characters with special escape sequences
instance Arbitrary SrcNamed where
  arbitrary = SrcNamed . format <$> elements ['\\', '\r', '\n', '\t']
    where
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
      named c = c `elem` "\r\n\t'\"\\"
      numbered c = c < '!' || c > '~'

-- Characters encoded as '\nnn;
instance Arbitrary SrcEscaped where
  arbitrary = SrcEscaped . format <$> arbitrary `suchThat` valid
    where
      format  = append "'\\" . flip (snoc . pack . show . ord) ';'
      valid c = not (named c) && numbered c
      named c = c `elem` "\r\n\t'\"\\"
      numbered c = c < '!' || c > '~'

-- Characters encoded as '\nnn; that don't need to be
instance Arbitrary SrcExcaped where
  arbitrary = SrcExcaped . format <$> arbitrary `suchThat` valid
    where
      format  = append "'\\" . flip (snoc . pack . show . ord) ';'
      valid c = named c || not (numbered c)
      named c = c `elem` "\r\n\t'\"\\"
      numbered c = c < '!' || c > '~'

instance Arbitrary SrcString where
  arbitrary = SrcString . format <$> arbitrary
    where
      format = cons '"' . flip snoc '"' . foldr (append . encode) ""
      escape = cons '\\' . flip append ";" . pack . show . ord
      encode '"'  = "\\\""
      encode '\n' = "\\n"
      encode '\r' = "\\r"
      encode '\t' = "\\t"
      encode '\\' = "\\\\"
      encode c
        | not $ isPrint c = escape c
        | not $ isAscii c = escape c
        | isSpace c       = escape c
        | otherwise       = cons c ""
