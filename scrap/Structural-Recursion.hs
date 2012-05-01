{-# LANGUAGE RankNTypes, TypeSynonymInstances, NoMonomorphismRestriction #-}

module SList
  ( null
  , cons
  , isNull
  , isCons
  , toList
  , fromList
  , map
  , head
  , append
  , sort
  , foldr
  , length
  ) where

import Prelude hiding (null, foldr, map, head, length, reverse, tail)
import qualified Data.List (foldr, map, reverse, length, sort)

---------------------------------------------------------------------------

import Test.QuickCheck

instance Arbitrary a => Arbitrary (SList a) where
  arbitrary = do xs <- arbitrary
                 return $ fromList xs

pfromList   = \xs -> (toList . fromList) xs == xs
ptoList     = \xs -> (fromList . toList) xs == xs
plength     = \xs -> length xs                  == Data.List.length (toList xs)
preverse    = \xs -> toList (reverse xs)        == Data.List.reverse (toList xs)
psort       = \xs -> toList (sort xs)           == Data.List.sort (toList xs)
pshow       = \xs -> show xs                    == show (toList xs)
pappend     = \xs ys -> toList (xs `append` ys) == toList xs ++ toList ys
peq         = \xs ys -> (xs == ys)              == (toList xs == toList ys)
pmap        = \f xs  -> toList (map f xs)       == Data.List.map f (toList xs)
pfoldr      = \f xs a -> foldr xs f a           == Data.List.foldr f a (toList xs)

main = do qc (pfromList   :: [Int]      -> Bool)
          qc (ptoList     :: SList Char -> Bool)
          qc (plength     :: SList Bool -> Bool)
          qc (preverse    :: SList Char -> Bool)
          qc (psort       :: SList Int  -> Bool)
          qc (pshow       :: SList Int  -> Bool)
          qc (pappend     :: SList Int  -> SList Int  -> Bool)
          qc (peq         :: SList Char -> SList Char -> Bool)
          qc (pmap (* 2)  :: SList Int  -> Bool)
          qc (pfoldr (*)  :: SList Int  -> Int -> Bool)
  where qc = quickCheck

---------------------------------------------------------------------------

-- Encode lists as their own foldr operation
newtype SList a = SList { foldr :: forall b. (a -> b -> b) -> b -> b }

-- Make them printable in the GHCi console
instance Show a => Show (SList a) where
  show as = let head = foldr as (\a tail -> ',':(show a) ++ tail) "]"
             in case head of
                  (',':tail) -> '[':tail
                  _          -> '[':head

-- Define equality
instance Eq a => Eq (SList a) where
  (==) as bs = lengthMatch && elemsMatch
    where
      lengthMatch = length as == length bs
      elemsMatch  = fst $ foldr as
                            (\a (ok, bs) -> (ok && a == head bs, tail bs))
                            (True, reverse bs)

-- Folding an empty list just returns the original seed value
null :: SList a
null = SList (\f seed -> seed)

-- Folding a non-empty list head:tail
cons :: a -> SList a -> SList a
cons head tail = SList (\f seed -> f head (foldr tail f seed))

-- Seed value is True, but changes to False if we fold over any elements at all
isNull :: SList a -> Bool
isNull as = foldr as (\a b -> False) True

-- Seed value is False, but changes to True if we fold over any elements at all
isCons :: SList a -> Bool
isCons as = foldr as (\a b -> True) False

-- Convert an SList to an ordinary Haskell list
toList :: SList a -> [a]
toList as = foldr as (:) []

-- Convert an ordinary Haskell list to an SList
fromList :: [a] -> SList a
fromList = Data.List.foldr cons null

map :: (a -> b) -> SList a -> SList b
map f as = foldr as (\a bs -> cons (f a) bs) null

-- Note: head null is undefined
head :: SList a -> a
head as = foldr as (\a b -> a) undefined

-- Note: tail null == null
tail :: SList a -> SList a
tail as = reverse . snd $ foldr (reverse as)
                            (\a (first, tail) -> (False, if first then tail else cons a tail))
                            (True, null)

append :: SList a -> SList a -> SList a
append as bs = foldr as cons bs

reverse :: SList a -> SList a
reverse as = foldr as (\a bs -> append bs (cons a null)) null

length :: Num b => SList a -> b
length as = foldr as (\a sum -> sum + 1) 0

sort :: Ord a => SList a -> SList a
sort as = foldr as (\a sorted -> insert sorted a) null
  where
    -- Inserts an element into a pre-sorted list,
    --   returning (as, as') where as' contains the new element x
    --   and all elements in as <= all elements in as'
    insert :: Ord a => SList a -> a -> SList a
    insert as x = snd $ foldr as (\a (rest, restE) ->
                               let rest'  = cons a rest
                                   restE' = if x <= a
                                            then cons x (cons a rest)
                                            else cons a restE
                                in (rest', restE'))
                           (null, cons x null)
