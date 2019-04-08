> module Chapters.Ch06 where
> import Euterpea
> import HSoM.Examples.MoreMusic

Chapter 6 - More Music: Delay, Repeat, Inversion, Retrograde, Polyrhythms,
Exercise 6.2
properRow :: Music Pitch -> Bool

> -- Exercise 6.3 
> palin :: Music Pitch -> Bool
> palin m = m == retro m
>
> -- Exercise 6.4
> -- retroPitches :: Music Pitch -> Music Pitch
> -- retroPitches m = let rp@(Prim(Note p o) d) = 
> --                  in map rp m
>
> --Exercise 6.5 
> test1 = (note hn (C,4)) /=: (note qn (C,4))
> test2 = forever (note qn (C,4)) /=: (note qn (C,4))
> test3 = (note hn (C,4)) /=: forever (note qn (C,4))

> --Exercise 6.6 
> --Define functions for mordent, turn and appogiatura

> -- roll  :: Dur -> Music Pitch -> Music Pitch
> -- rolln :: Int -> Music Pitch -> Music Pitch

> --  roll  dur    m = trill  0 dur m
> --  rolln nTimes m = trilln 0 nTimes m

> -- from hsom; example Percussion notation
> funkGroove:: Music Pitch
> funkGroove
>    = let p1 = perc LowTom qn
>          p2 = perc AcousticSnare en
>       in tempo 2 $ instrument Percussion $ cut 8 $ forever
>           ((p1:+:qnr:+:p2:+:qnr:+:p2:+:p1:+:p1:+:qnr:+:p2:+:enr)
>               :=: roll en (perc ClosedHiHat 2))

> -- Exercise 6.7
> -- allPerc :: Music Pitch
> --allPerc = let p1 = 

Exercise 6.8
Make a beat

Exercise 6.9 
Define scaleVolume

Exercise 6.10
Redefine revM using mFold

Exercise 6.11
Define insideOut

