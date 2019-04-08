> module Chapters.Ch02 where
> import Euterpea
> import Chapters.Ch01

Chapter 2 - Simple Music: Notes, Absolute pitches ...

Exercise 2.1

>
> t251 :: Music Pitch
> t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
>            gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
>            cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
>        in dMinor :+: gMajor :+: cMajor
>
> minorTriad :: Dur -> Pitch -> Music Pitch
> minorTriad d p = note d p :=: hNote d (trans 3 p) :=: hNote d (trans 7 p)
> majorTriad :: Dur -> Pitch -> Music Pitch
> majorTriad d p = note d p :=: hNote d (trans 4 p) :=: hNote d (trans  7 p)
> twoFiveOne :: Pitch -> Dur -> Music Pitch
> twoFiveOne p d = let minorTwo = minorTriad d (trans 2 p)
>                      majorFive = majorTriad d (trans 7 p)
>                      majorOne = majorTriad (2*d) p
>                   in minorTwo :+: majorFive :+: majorOne
>

twoFiveOne (C,4) wn = t251
-- substitute the abstractions for the functions they refer to
=> minorTwo = minorTriad wn (trans 2 (C,4))
   majorFive = majorTriad wn (trans 7 (C,4))
   majorOne = majorTraid (wn*2) (C,4)
=> minorTwo = minorTriad wn (D,4)
   majorFive = majorTriad wn (G,4)
   majorOne = majorTriad wn (C,4)
=> minorTwo = note wn (D,4) :=: hNote wn 3 (D,4) :=: hNote wn 7 (D,4)
   majorFive = wn (G,4) :=: hNote wn 4 (G,4) :=: hNote wn 7 (G,4)
   majorOne = wn (C,4) :=: hNote wn 4 (C,4) :=: hNote wn 7 (C,4)
   -- Euterpea wraps the octave numbers at A, not C
=> minorTwo = note wn (D,4) :=: note wn (F,4) :=: note wn (A,5)
   majorFive = note wn (G,4) :=: note wn (B,5) :=: note wn (D,5)
   majorOne = note wn (C,4) :=: note wn (E,4) :=: note wn (G,4)
=> t251

> -- Exercise 2.2-1
> -- BluesPitchClass data type
> data BluesPitchClass = Ro | MT | Fo | Fi | MS
>
> -- Exercise 2.2-2
> type BluesPitch = (BluesPitchClass, Octave)
>
> -- helper functions that translate from an inline delaration to a typed note
> -- see line 177 of Euterpea.Music
>
> -- Exercise 2.2-3
> ro, mt, fo, fi, ms :: Octave -> Dur -> Music BluesPitch
> ro o d = note d (Ro, o)
> mt o d = note d (MT, o)
> fo o d = note d (Fo, o)
> fi o d = note d (Fi, o)
> ms o d = note d (MS, o)
>
> -- Exercise 2.2-4
>
> -- Converting BluesPitch to Pitch
> -- simlar declarations line 22 of Euterpea.Music
> -- designates a path by which various permutations are translated
>
> fromBlues :: Music BluesPitch -> Music Pitch
> fromBlues (Prim (Note d (p,o))) = case p of
>     Ro -> note d (C,o)
>     MT -> note d (Ef,o)
>     Fo -> note d (F,o)
>     Fi -> note d (G,o)
>     MS -> note d (Bf,o)
> fromBlues (Prim (Rest d)) = rest d
> fromBlues (m1 :+: m2) = (fromBlues m1 :+: fromBlues m2)
> fromBlues (m1 :=: m2) = (fromBlues m1 :=: fromBlues m2)
> fromBlues (Modify ctrl m1) = Modify ctrl (fromBlues m1)
>
> -- Exercise 2.2-5
> -- need to use the fromBlues helper functions to go from BluesPitch -> Pitch,
> -- which the play function can utilise

> melody1 :: Music BluesPitch
> melody1 = ro 4 qn :+: ro 5 qn :+: mt 4 qn :+: fo 4 qn :+: fi 4 qn :+: ms 3 qn
>           :+: ro 4 qn
> test_play1 = play (fromBlues melody1)
>
> -- Exercise 2.3
> -- Don't think this one works in Euterpea2
> -- abspitch (pitch ap) = ap
> -- => abspitch Pitch = ap 
> -- pitch (abspitch p) = p
>

Exercise 2.4
Show that trans i (trans j p) = trans (i +j) p.
trans i (pitch (absPitch p + i))
pitch (absPitch (pitch (absPitch p + j)) +i)
????
pitch ((absPitch p) + j + i)
trans (i + j) p

> -- Exercise 2.5
> transM :: AbsPitch -> Music Pitch -> Music Pitch
> transM ap (Prim (Note d p)) = note d (trans ap p)
> transM ap (Prim (Rest d)) = rest d
> transM ap (m1 :+: m2) = (transM ap m1) :+: (transM ap m2)
> transM ap (m1 :=: m2) = (transM ap m1) :=: (transM ap m2)
> transM ap (Modify ctrl m1) = Modify ctrl (transM ap m1)

