> module Chapters.Ch07 where
> import Euterpea

Chapter 7 - Qualified Types, Type Classes

Exercise 7.1 Prove that the instance of Music in class Eq satisfies the laws of
its class.

Exercise 7.2
Write out appropriate instance declarations for the Color type in the classes 

Exercise 7.3 Define a type class called Temporal whos members are types that can be
interpreted as having a temporal duration.


> trill :: Int -> Dur -> Music Pitch -> Music Pitch
> trill i sDur (Prim (Note tDur p)) =
>    if sDur >= tDur  then note tDur p
>                     else  note sDur p :+: 
>                           trill  (negate i) sDur 
>                                  (note (tDur-sDur) (trans i p))
> trill i d (Modify (Tempo r) m)  = tempo r (trill i (d*r) m)
> trill i d (Modify c m)          = Modify c (trill i d m)
> trill _ _ _                     = 
>       error "trill: input must be a single note."
> trill' :: Int -> Dur -> Music Pitch -> Music Pitch
> trill' i sDur m = trill (negate i) sDur (transpose i m)
> trilln :: Int -> Int -> Music Pitch -> Music Pitch
> trilln i nTimes m = trill i (dur m / fromIntegral nTimes) m
> trilln' :: Int -> Int -> Music Pitch -> Music Pitch
> trilln' i nTimes m = trilln (negate i) nTimes (transpose i m)
> roll  :: Dur -> Music Pitch -> Music Pitch
> rolln :: Int -> Music Pitch -> Music Pitch
> 
> roll  dur    m = trill  0 dur m
> rolln nTimes m = trilln 0 nTimes m
