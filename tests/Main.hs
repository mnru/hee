{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Language.Hee.Tests.Syntax  as Syntax (tests)
import Language.Hee.Tests.Unify   as Unify (tests)

import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain . concat $
  [ Syntax.tests
  , Unify.tests
  ]
