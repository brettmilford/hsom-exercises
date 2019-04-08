> module Chapters.Ch05 where
> import Euterpea

Chapter 5 - Sections, Anon Functions, List Comprehensions, Function composition
Chapter 5

> -- Exercise 5.1
> twice :: (a -> a) -> a -> a
> twice f a = f (f a)
>
> -- Exercise 5.2
> -- power :: (a -> a) -> Int -> (a -> a)
> -- power f 0 = f
> -- power f n = f (power f n-1)

> --Exercise 5.3
> --Rewrite remainder using fix so that it is not recursive.
> fix f = f(fix f)
>
> -- remainder :: Integer -> Integer -> Integer
> -- remainder a b = if a < b then a
> --                 else fix remainder
> -- Exercise 5.4
> -- apPairs :: [AbsPitch] -> [AbsPitch] -> [(AbsPitch,AbsPitch)]
> -- apPairs aps1 aps2 = [(ap1,ap2) | ap1 <- aps1, ap2 <- aps2, (ap1 - ap2) > 2]
> -- apPairsMusic :: [(AbsPitch,AbsPitch)] -> Music a
> -- apPairsMusic abs = line (map harm abs) -- doesn't have durations??..
> --                   where harm (a,b) = pitch a :=: pitch b
> -- Exercise 5.5
> -- hList d ps = (line . map (hNote d)) ps
> -- hList' d = line . map (hNote d)

Is there a way to futher curry hList'?

> -- Exercise 5.6
> -- addDur :: Dur -> [Dur -> Music a] -> Music a
> -- addDur d = line . map $ f

> --Exercise 5.7
> --map (\x -> (x+1)/2) xs -- rewrite as a composition of sections
> --exercise5_7 =  map (+1).(/2) xs

> -- Exercise 5.8
> -- exercise5_8_1 = map f.g xs
> -- exercise5_8_2 = map (+1) (map (/2) xs)

Exercise 5.10
f1 (f2 (*) [1,2,3,4]) 5 => [5,10,15,20]

map (*) [1,2,3,4]
map (\x -> x *) [] ???

