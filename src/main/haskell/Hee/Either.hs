{-# LANGUAGE FlexibleInstances #-}
module Hee.Either where

instance Monad (Either String) where
  return        = Right
  Right r >>= f = f r
  Left l  >>= _ = Left l
  fail msg      = Left msg
