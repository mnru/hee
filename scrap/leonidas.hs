{-# LANGUAGE TypeOperators, NoImplicitPrelude #-}

-- https://github.com/leonidas/codeblog/blob/master/2012/2012-02-17-concatenative-haskell.md

import qualified Data.List as L
import Control.Arrow
import Control.Monad
import Prelude (Bool, Int, Float, fst, snd, flip, ($), uncurry, Show(..), String, (.), otherwise, IO)
import qualified Prelude as P
import qualified Data.Tuple

data s :. a = !s :. !a
infixl 1 :.

instance (P.Show s, P.Show a) => P.Show (s:.a) where
  show (s:.a) = (P.++) ((P.++) (P.show s) " ") (P.show a)

push :: a -> s -> s:.a
push a s = s:.a

q :: a -> s -> s:.a
q = push

dup :: s:.a -> s:.a:.a
dup (s:.a) = (s:.a:.a)

swap :: s:.a:.b -> s:.b:.a
swap (s:.a:.b) = s:.b:.a

add :: P.Num n => s:.n:.n -> s:.n
add (s:.a:.b) = s:.a P.+ b

addTwo = push 2 >>> add

liftS :: (a -> b) -> (s:.a -> s:.b)
liftS f (s:.a) = s:.f a

liftS2 :: (a -> b -> c) -> (s:.a:.b -> s:.c)
liftS2 f (s:.a:.b) = s:.f a b

(+) :: P.Num n => s:.n:.n -> s:.n
(+) = liftS2 (P.+)

(-) :: P.Num n => s:.n:.n -> s:.n
(-) = liftS2 (P.-)

(*) :: P.Num n => s:.n:.n -> s:.n
(*) = liftS2 (P.*)

(/) :: P.Fractional n => s:.n:.n -> s:.n
(/) = liftS2 (P./)

(<) :: P.Ord n => s:.n:.n -> s:.Bool
(<) = liftS2 (P.<)

(>) :: P.Ord n => s:.n:.n -> s:.Bool
(>) = liftS2 (P.>)

(>=) :: P.Ord n => s:.n:.n -> s:.Bool
(>=) = liftS2 (P.>=)

(<=) :: P.Ord n => s:.n:.n -> s:.Bool
(<=) = liftS2 (P.<=)

(==) :: P.Eq n => s:.n:.n -> s:.Bool
(==) = liftS2 (P.==)

(++) :: s:.[a]:.[a] -> s:.[a]
(++) = liftS2 (P.++)

null :: s:.[a] -> s:.Bool
null = liftS P.null

decons :: s:.[a] -> s:.[a]:.a
decons (s:.(x:xs)) = s:.xs:.x

cons :: s:.[a]:.a -> s:.[a]
cons = liftS2 (flip (:))

drop :: s:.a -> s
drop (s:.a) = s

drop2 :: s:.a:.b -> s
drop2 = drop . drop

drop3 :: s:.a:.b:.c -> s
drop3 = drop . drop2

nip :: s:.a:.b -> s:.b
nip (s:.a:.b) = s:.b

nip2 :: s:.a:.b:.c -> s:.c
nip2 = nip . nip

over :: s:.a:.b -> s:.a:.b:.a
over (s:.a:.b) = (s:.a:.b:.a)

pick :: s:.a:.b:.c -> s:.a:.b:.c:.a
pick (s:.a:.b:.c) = (s:.a:.b:.c:.a)

dupd :: s:.a:.b -> s:.a:.a:.b
dupd (s:.a:.b) = (s:.a:.a:.b)

swapd :: s:.a:.b:.c -> s:.b:.a:.c
swapd (s:.a:.b:.c) = s:.b:.a:.c

rotl :: s:.a:.b:.c -> s:.b:.c:.a
rotl (s:.a:.b:.c) = s:.b:.c:.a

rotr :: s:.a:.b:.c -> s:.c:.a:.b
rotr (s:.a:.b:.c) = s:.c:.a:.b

apply :: s:.(s -> s') -> s'
apply (s:.f) = f s

dip :: s:.a:.(s -> s') -> s':.a
dip (s:.a:.f) = f s :. a

dip2 :: s:.a:.b:.(s -> s') -> s':.a:.b
dip2 (s:.a:.b:.f) = f s :. a :. b

keep :: s:.a:.(s:.a -> s') -> s':.a
keep (s:.a:.f) = f (s:.a) :. a

keep2 :: s:.a:.b:.(s:.a:.b -> s') -> s':.a:.b
keep2 (s:.a:.b:.f) = f (s:.a:.b) :. a :. b

length :: s:.[a] -> s:.Int
length = liftS P.length

reverse :: s:.[a] -> s:.[a]
reverse = liftS P.reverse

take :: s:.[a]:.Int -> s:.[a]
take = liftS2 $ flip P.take


if_ :: s:.Bool:.(s -> s'):.(s -> s') -> s'
if_ (s:.cond:.then_:.else_)
    | cond      = then_ s
    | otherwise = else_ s

map :: s:.[a]:.(s:.a -> s:.b) -> s:.[b]
map (s:.lst:.f) = (uncurry (:.)) (L.mapAccumL step s lst) where
    step s x = let s:.y = f (s:.x) in (s,y)

foldr :: s:.[x]:.acc:.(s:.acc:.x -> s:.acc) -> s:.acc
foldr = pick
    >>> null
    >>> q( drop >>> nip )
    >>> q( q decons
        >>> dip2
        >>> rotl
        >>> q( q foldr >>> keep ) >>> dip
        >>> swap
        >>> apply
        )
    >>> if_

foldl :: s:.[x]:.acc:.(s:.x:.acc -> s:.acc) -> s:.acc
foldl = pick
    >>> null
    >>> q( drop >>> nip )
    >>> q( q( decons >>> swap )
        >>> dip2
        >>> rotl
        >>> q( q apply >>> keep )
        >>> dip
        >>> rotr
        >>> foldl
        )
    >>> if_

filter :: s:.[x]:.(s:.x -> s:.Bool) -> s:.[x]
filter = swap >>> push [] >>> q step >>> foldr >>> nip where
    step = rotr
        >>> pick
        >>> q( q( apply >>> q( q cons ) >>> q( q drop ) >>> if_ )
                >>> keep
            ) >>> dip2
        >>> q rotl >>> dip
        >>> swap
        >>> apply

putStr :: s:.String -> IO s
putStr (s:.x) = P.putStr x >> return s

putStrLn :: s:.String -> IO s
putStrLn (s:.x) = P.putStrLn x >> return s

getLine :: s -> IO (s:.String)
getLine s = P.getLine >>= \ln -> return (s:.ln)

hello = push "What's your name? " >>> return
    >=> putStr
    >=> push "Hello, " >>> return
    >=> getLine
    >=> (++) >>> push "!" >>> (++) >>> return
    >=> putStrLn

exampleStack :: s -> s:.String:.Int
exampleStack = push "foo"
           >>> push 100

testFilter = push [1..20]
    >>> q( push 5 >>> (>) )
    >>> filter
    >>> push 5 >>> take

applyTest = push 2 >>> q( push 3 >>> (+) ) >>> apply

test = push 1 >>> push 2 >>> add
