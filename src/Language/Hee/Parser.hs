{-# LANGUAGE OverloadedStrings #-}

module Language.Hee.Parser
  (
  ) where

import Control.Applicative

import Data.Attoparsec.Text
import Language.Hee.Terms

heeTerm   :: Parser Term
heeQuote  :: Parser Term
heeChar   :: Parser Term
heeString :: Parser Term
heeNumber :: Parser Term

heeExpr   = undefined
heeTerm   = undefined
heeQuote  = undefined
heeChar   = undefined
heeString = undefined
heeNumber = undefined
