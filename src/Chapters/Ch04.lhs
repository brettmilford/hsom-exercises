> module Chapters.Ch04 where
> import Euterpea
> import HSoM.Examples.Interlude

Chapter 4 - Interlude: transcribing, simple algorithmic compositions
Helper functions
Adds a duration to a list of notes:

> -- borrowed from  module  HSoM.Examples.Interlude
> addDur       :: Dur -> [Dur -> Music a] -> Music a
> addDur d ns  =  let f n = n d
>                 in line (map f ns)

Adds a grace note to a primary note:

> graceNote :: Int -> Music Pitch -> Music Pitch
> graceNote n  (Prim (Note d p))  =
>           note (d/8) (trans n p) :+: note (7*d/8) p
> graceNote n  _                  = 
>           error "Can only add a grace note to a note."

A transcription of Chick Corea's Children's Song No.6.
Base lines:

> b1  = addDur dqn [b 2,   fs 3,  g 3,   fs 3]
> b2  = addDur dqn [b 2,   es 3,  fs 3,  es 3]
> b3  = addDur dqn [as 2,  fs 3,  g 3,   fs 3]

> bassLine =  times 3 b1 :+: times 2 b2 :+: 
>             times 4 b3 :+: times 5 b1

Top line, mainline, mainvoice etc:

> mainVoice = times 3 v1 :+: v2

> v1   = v1a :+: graceNote (-1) (d 4 qn) :+: v1b                 --  bars 1-2
> v1a  = addDur en [a 4, e 4, d 4, fs 4, cs 4, b 3, e 4, b 3]
> v1b  = addDur en [cs 4, b 3]

> v2 = v2a :+: v2b :+: v2c :+: v2d :+: v2e :+: v2f :+: v2g

> v2a  =  line [  cs 4 (dhn+dhn), d 4 dhn, 
>                 f 4 hn, gs 4 qn, fs 4 (hn+en), g 4 en]     --  bars 7-11
> v2b  =  addDur en [  fs 4, e 4, cs 4, as 3] :+: a 3 dqn :+:
>         addDur en [  as 3, cs 4, fs 4, e 4, fs 4]          --  bars 12-13
> v2c  =  line [  g 4 en, as 4 en, cs 5 (hn+en), d 5 en, cs 5 en] :+:
>         e 4 en :+: enr :+: 
>         line [  as 4 en, a 4 en, g 4 en, d 4 qn, c 4 en, cs 4 en] 
>                                                            --  bars 14-16
> v2d  =  addDur en [  fs 4, cs 4, e 4, cs 4, 
>                      a 3, as 3, d 4, e 4, fs 4]            --  bars 17-18.5
> v2e  =  line [  graceNote 2 (e 4 qn), d 4 en, graceNote 2 (d 4 qn), cs 4 en,
>                 graceNote 1 (cs 4 qn), b 3 (en+hn), cs 4 en, b 3 en ]  
>                                                            --  bars 18.5-20
> v2f  =  line [  fs 4 en, a 4 en, b 4 (hn+qn), a 4 en, fs 4 en, e 4 qn,
>                 d 4 en, fs 4 en, e 4 hn, d 4 hn, fs 4 qn]  --  bars 21-23
> v2g  =  tempo (3/2) (line [cs 4 en, d 4 en, cs 4 en]) :+: 
>         b 3 (3*dhn+hn)                                     --  bars 24-28

Main composing function, sets tempo, instrument & notes; to be used with play
function.

> childSong6 :: Music Pitch
> childSong6 =  let t = (dhn/qn)*(69/120)
>               in instrument  RhodesPiano 
>                              (tempo t (bassLine :=: mainVoice))


> -- Exercise 4.1
> -- Bach Chorale No. 76
> -- SATB Score, 14 bars, 1-5 repeated
> -- helper funcs
> repeatMusic :: Int -> Music a -> Music a
> repeatMusic 1 m = m
> repeatMusic r m = m :+: repeatMusic (r-1) m

> --addDur :: Dur -> [Dur -> Music a] -> [Music a]
> --addDur d [m] = [m d]
> --addDur d (m:ms) = (m d) : addDur d ms

> --addDur' :: Dur -> [Dur -> Music a] -> Music a
> --addDur' _ [] = []
> --addDur' d (m:ms) = line (m d: addDur d ms)

> addDur'' :: Dur -> [Dur -> Music a] -> Music a
> addDur'' d ms = line (map f ms)
>               where f m = m d
> -- phrases
> bas1 = line [g 3 qn, fs 3 qn, g 3 qn, a 3 qn, b 3 en, cs 4 en, d 4 qn, g 3 qn,
>               a 3 qn, phrase [Art Fermata] (d 3 hn)]
> bas2 = line [e 3 qn, f 3 qn, g 3 qn, a 3 qn, b 3 en, c 4 en, d 4 en, d 3 en, 
>               phrase [Art Fermata] (g 2 wn)]
> bas3 = line [g 2 qn, a 2 qn, b 2 qn, c 3 en, d 3 en, e 3 qn, f 3 qn,
>               phrase [Art Fermata] (e 3 hn)]
> bas4 = line [b 2 en, a 2 en, g 2 qn, c 3 qn, d 3 qn, e 3 en, fs 3 en, g 3 qn,
>               phrase [Art Fermata] (d 3 hn)]
> bas5 = line [g 3 qn, g 3 en, fs 3 en, e 3 qn, ds 3 qn, e 3 en, d 3 en, c 3 qn,
>               g 3 en, a 3 en, b 3 qn, phrase [Art Fermata] (e 3 hn)]
> bas6 = line [e 3 qn, fs 3 en, gs 3 en, a 3 qn] :+: addDur'' en [b 3, c 4, d 3,
>               c 3, b 2, g 2] :+: line [d 3 hn, phrase [Art Fermata] (g 2 hn)]
> ten1 = line [b 3 qn, d 4 qn, d 4 en, e 4 en, e 4 en, fs 4  en, b 3 en, a 3 en,
>               a 3 qn, b 3 qn, a 3 en, g 3 en, phrase [Art Fermata] (fs 3 hn)]
> ten2 = line [b 3 qn, a 3 qn, a 3 en, g 3 en, g 3 en, fs 3 en, b 3 en, c 4 en,
>               d 4 en, c 4 en, phrase [Art Fermata] (b 3 wn)]
> ten3 = line [d 4 qn, a 3 qn, a 3 en, gs 3 en, a 3 qn, e 3 en, e 4 qn, d 4 en,
>               phrase [Art Fermata] (gs 3 hn)]
> ten4 = line [g 3 en, a 3 en, b 3 qn] :+: addDur'' en [b 3, a 3, a 3, b 3, b 3,
>              a 3, b 3, cs 4] :+: phrase [Art Fermata] (d 4 hn)
> ten5 = line [d 4 qn, d 4 qn, g 3 en, a 3 en, b 3 qn, b 3 qn, c 4 en, a 3 en, e
>               4 dqn, ds 4 en, phrase [Art Fermata] (b 3 hn)]
> ten6 = line [e 4 qn, a 3 en, b 3 en] :+: addDur'' qn [c 4, d 4, d 4, d 4] :+:
>        line [d 4 dqn, c 4 en, phrase [Art Fermata] (b 3 hn)]
> alt1 = line [d 4 qn, d 4 qn, g 4 qn, cs 4 qn, d 4 en, e 4 en, e 4 en, d 4 en,
>               d 4 qn, cs 4 qn, phrase [Art Fermata] (d 4 hn)]
> alt2 = line [e 4 qn, e 4 en, d 4 en, d 4 qn, a 4 qn, a 4 en, g 4 en, g 4 en,
>               fs 4 en, phrase [Art Fermata] (d 4 wn)]
> alt3 = line [g 4 qn, g 4 en, f 4 en, f 4 qn, e 4 en, fs 4 en, gs 4 qn, a 4 en,
>               a 3 en, phrase [Art Fermata] (e 4 hn)]
> alt4 = line [d 4 qn, g 4 en, f 4 en, e 4 qn, fs 4 qn, g 4 en, d 4 qn, e 4 en,
>               phrase [Art Fermata] (fs 4 hn)]
> alt5 = addDur'' qn [g 4, a 4, g 4, fs 4] :+:
>                   addDur'' en [fs 4, e 4, e 4, fs 4] :+:
>                   line [g 4 qn, fs 4 qn, phrase [Art Fermata] (e 4 hn)]
> alt6 = line [gs 4 qn, d 4 en, e 4 en, e 4 en, fs 4 en, g 4 qn, fs 4 qn,
>               g 4 qn, g 4 en, -- could use dotted note here
>               fs 4 sn, e 4 sn, fs 4 qn, phrase [Art Fermata] (d 4 hn) ]

> sop1 = line [g 4 qn, a 4 qn, b 4 qn, a 4 qn, g 4 qn, fs 4 qn, e 4 hn,
>               phrase [Art Fermata] (d 4 hn)]
> sop2 = line [g 4 qn, a 4 qn, b 4 qn, c 5 qn, b 4 qn, a 4 qn,
>               phrase [Art Fermata] (g 4 wn)]
> sop3 = addDur'' qn [b 4, c 5, d 5, c 5, b 4, a 4]
>                   :+: phrase [Art Fermata] (b 4 hn)
> sop4 = addDur'' qn [d 5, d 5, e 5, d 5, c 5, b 4]
>                   :+: phrase [Art Fermata] (a 4 hn)
> sop5 = line [b 4 en, c 5 en, d 5 qn, c 5 qn, b 4 en, a 4 en, g 4 qn, a 4 qn, 
>               b 4 hn, phrase [Art Fermata] (g 4 hn) ]
> sop6 = line [b 4 en, c 5 en, d 5 qn, c 5 qn, b 4 qn, a 4 qn, b 4 den,
>               c 5 sn, a 4 hn, phrase [Art Fermata] (g 4 hn)]

> sop = repeatMusic 2 (sop1 :+: sop2) :+: sop3 :+: sop4 :+: sop5 :+: sop6
> alto = repeatMusic 2 (alt1 :+: alt2) :+: alt3 :+: alt4 :+: alt5 :+: alt6
> ten = repeatMusic 2 (ten1 :+: ten2) :+: ten3 :+: ten4 :+: ten5 :+: ten6
> bass = repeatMusic 2 (bas1 :+: bas2) :+: bas3 :+: bas4 :+: bas5 :+: bas6

> chroale76 :: Music Pitch
> chroale76 = let temp = (90/120)
>             in instrument ChurchOrgan 
>                           (tempo temp (sop :=: alto :=: ten :=: bass))

There seems to be a lack of:
    1) Supoprt for "tied" notes
        - Could use a dotted note in place but doesn't always work
    2) Support for tripplet (and tupplet?) notes
        - faked via tempo maths

Exercise 4.2
An example of algorithmic composition. Takes a motif (see mel1 & mel2) and
generates every proper (non-empty) prefix of the given melody.

Returns all proper prefixes of a list:

> prefixes         :: [a] -> [[a]]
> prefixes []      =  []
> prefixes (x:xs)  =  let f pf = x:pf
>                     in [x] : map f (prefixes xs)

> prefix :: [Music a] -> Music a
> prefix mel =  let  m1  = line (concat (prefixes mel))
>                    m2  = transpose 12 (line (concat (prefixes (reverse mel))))
>                    m   = instrument Flute m1 :=: instrument VoiceOohs m2
>               in m :+: transpose 5 m :+: m

> mel1 = [c 4 en, e 4 sn, g 4 en, b 4 sn, a 4 en, f 4 sn, d 4 en, b 3 sn, c 4 en]
> mel2 = [c 4 sn, e 4 sn, g 4 sn, b 4 sn, a 4 sn, f 3 sn, d 4 sn, b 3 sn, c 4 sn]

This function composes the augmentations as follows:
m1 = sequential line on flute
m2 = sequential line 8va on Voice
played in parallel, followed by the same transposed a perfect 4th, and again in
the original.

Can be performed with `play (line (concat (prefixes mel)))`
note. The formation in the last prefixes match, seems to create a list with the
first note, than the first two notes, than the first 3, 4, etc. Until the end.
Similar function structure found at the addDur function.

Exercise 4.3
use different instruments, change m, compose the result a different way?


