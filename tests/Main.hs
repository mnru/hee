{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Language.Hee.Tests.Syntax as A (tests)

import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain . concat $
  [ A.tests
  ]
