{-# LANGUAGE FlexibleInstances #-}
module Hee.Either where

instance Functor (Either l) where
  fmap f (Right x) = Right (f x)
  fmap _ (Left x)  = Left x

instance Monad (Either String) where
  return        = Right
  Right r >>= f = f r
  Left l  >>= _ = Left l
  fail msg      = Left msg
