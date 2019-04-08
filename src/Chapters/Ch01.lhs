> module Chapters.Ch01 where
> import Euterpea

Chapter 1 - Principles, expressions, types, functions, abstraction

Exercise 1.1 p8
Exercise 1.2
Exercise 1.3 p13

Exercise 1.4

> hNote :: Dur -> Pitch -> Int -> Music Pitch
> hNote d p s = note d p :=: note d (trans (s) p)
>
> hList :: Dur -> [Pitch] -> Int -> Music Pitch
> hList d [] s = rest 0
> hList d (p:ps) s = hNote d p s :+: hList d ps s
>
> mel :: Music Pitch
> mel = hNote qn p1 (-3) :+: hNote qn p2 (-3) :+: hNote qn p3 (-3)
> p1 = (C,4)
> p2 = (E,4)
> p3 = (D,4)
>
