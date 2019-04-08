> module Chapters.Ch03 where
> import Euterpea

Chapter 3 - Polymorphic / Higher-Order functions

> -- Exercise 3.1.1
> f1 :: Int -> [Pitch]-> [Pitch]
> f1 s ps = map (trans s) ps
>
> -- This would also work, but probably not as suscinct
> -- f1 ap (x:xs) = (trans ap x):f1 ap xs

> -- Exercise 3.1.2
> f2 :: [Dur] -> [Music a]
> f2 ds = map rest ds
>
> --Or?
> -- f2 (d:ds) = rest d : f2 ds
>
> {-- Exercise 3.1.3
> This is a tricky one, the let statement basically defines a function (f) that is
> passed to map at the in statement which basically splits apart the Music a
> structure and creates a note and a reset from it. *check scaleDuration from
> Euterpea.Music --}
>
> f2' :: [Music Pitch] -> [Music Pitch]
> f2' mp = let f (Prim (Note d (p,o))) = note (d/2) (p,o) :+: rest (d/2)
>           in map f mp
>

Music a => Prim (Primitive a)
=> Prim (Note Dur a) || Prim (Rest Dur)
=> Prim (Note d p)
=> Prim (Note d (p,o))
There are some (many?) steps missing in this..

Note well, when passing an operator to fold, surround it in parens ()

Exercise 3.2
Show that flip (flip f) is the same as f

flip :: (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x 

=> flip (flip f)
=> flip (flip f x y)
=> flip f y x
=> f x y => f

Exercise 3.3
ys::[(+)] => a list of `+` functions ?

Exercise 3.4

> applyEach :: ([a -> a]) -> a -> [a]
> applyEach [] _ = []
> applyEach (x:xs) i = x i : applyEach xs i

OR use fold?

> applyEach2 :: [(a -> a)] -> a -> [a]
> applyEach2 [] _ = []
> applyEach2 (f:fs) i = foldr f i

Exercise 3.5

> applyAll :: [(a -> a)] -> a -> a
> applyAll (f:fs) x = let x = map f x
>                     in applyAll fs x

Exercise 3.6
appendr,appendl::[[a]] -> [a]
appendr = foldr (flip (++)) []
appendl = foldl (flip (++)) []

???

Exercise 3.7

> length1 :: [a] -> Integer
> length1 xs = let add x y = 1 + y
>              in foldr (add) 0 xs

Exercise 3.8
a. Doubles each number in a list.

> doubleEach :: [Integer] -> [Integer]
> --doubleEach xs = foldr (*2) 0 xs
> doubleEach = foldr (*2) 0

b. Pairs each elements in a list with that number and one plust that number.

> pairAndOne :: [Integer] -> [(Integer,Integer)]
> pairAndOne xs = let pairAndOne' (y:ys) z = pairAndOne' ys (y,y+1):z
>                     pairAndOne' [] as = as
>                  in pairAndOne' xs _
> --pairAndOne (x:xs) = pairAndOne (xs:(x:(x+1))
> --pairAndOne [x]    = (x:(x + 1))

c. Adds together each pair of number in a list.

> addEachPair :: [(Int,Int)] -> [Int]
> addEachPair ((x,y):xys) = let z = x + y
>                           in z : addEachPair xys

d. Add "poiontwise" the elements of a list of pairs.

> addPairsPointwise :: [(Int, Int)] -> (Int,Int)
> addPairsPointwise ((x,y):xys) = let addPairsPointwise' [] (c,d) = (c,d)
>                                     addPairsPointwise' ((a,b):xs) (c,d) = addPairsPointwise' xs (a+c, b+d)
>                                 in addPairsPointwise' xys (x,y)

Exercise 3.9
Combines a list of durations with a list of notes lacking a duration to craete a
list of complete notes

> fuse :: [Dur] -> [Dur -> Music a] -> [Music a]
> fuse (d:ds) (m:ms) = (m d) : fuse ds ms
> fuse [] [] = []
> fuse _ [] = error "List of Dur too long"
> fuse [] _ = error "List of Music too long"


Exercise 3.10

> maxAbsPitch :: [AbsPitch] -> AbsPitch
> maxAbsPitch (a:as) = maxAbsPitch' as a
>                      where maxAbsPitch' (a:as) p
>                              | a > p = maxAbsPitch' as a
>                              | p > a = maxAbsPitch' as p
>                            maxAbsPitch' [] p = p

> --minAbsPitch :: [AbsPitch] -> AbsPitch

Exercise 3.11

> chrom :: Pitch -> Pitch -> Music Pitch
> chrom p1 p2 | p1 == p2 = note 4 p1
>             | p1 > p2 = chrom' p1 p2
>             | p1 < p2 = chrom'' p1 p2
>
>-- chrom' p1 p2 | p1 /= p2 = note 4 p1 :=: chrom' (p1+1) p2

Exercise 3.12

> mkScale :: Pitch -> [Int] -> Music Pitch
> mkScale p ints =  
>           let mkScale' p (i:is) music = mkScale' p is music :=: note p+i 4
>               mkScale' _ [] music = music
>           in mkScale' p ints note p 4

Exercise 3.13

> data ScaleType = Ionian | Dorian | Phrygian
>                | Lydian | Mixolydian | Aeolian
>                | Locrian

> genScale :: Pitch -> ScaleType -> Music Pitch
> genScale p sc = error "Exercises.genScale: Not implimented"

Exercise 3.14

> frere :: Music Pitch
> frere = p1 :+: p2 :+: p3 :+: p4
> twice x = x :+: x
> p1 = twice (c 4 qn :+: d 4 qn :+: e 4 qn :+: c 4 qn)
> p2 = twice (e 4 qn :+: f 4 qn :+: g 4 hn)
> p3 = twice (g 4 en :+: a 4 en :+: g 4 en :+: f 4 en :+: e 4 qn :+: c4 qn)
> p4 = twice (c 4 qn :+: g 3 qn :+: c 4 hn)

Multiplies a duration

> multDur :: Dur -> Integer -> Dur
> mutlDur d 1 = 0
> multDur d i = d + multDur d (i-1)

Creates a round, takes the number of voices and the duration by which they
should be separated

> mround :: Int -> Dur -> Music Pitch -> Music Pitch
> mround 1 d m = m
> mround v d m = (rest (multDur d (v-1)) :+: m) :=: (mround d (v-1) m)

Returns the round as a list

> mroundl :: Int -> Dur -> Music Pitch -> [Music Pitch]
> mroundl 1 d m = [m]
> mroundl v d m = (rest (multDur d (v-1)) :+: m) : (mround' d (v-1) m)

Assign instruments to voices

> assignInst :: [InstrumentName] -> [Music Pitch] -> [Music Pitch]
> assignInst (i:is) (m:ms) = instrument i m : assignInst is ms
> assignInst [] [] = []
> assignInst _ [] = error "too many instruments"
> assignInst [] _ = error "too many musics"

Play

> brassQuartet = [Tuba,Trombone,FrenchHorn,Trumpet]
> frereRound = mroundl bn 4 frere
> frereRoundInstruments = assignInst brassQuartet frereRound
> playFrere = play (foldl (:=:) (rest qn) frereRoundInstruments)

Exercise 3.15

> encrypt :: [Char] -> [Char]
> encrypt (c:cs) =
>               let encrypt' (c:cs) ds = 
>                       encrypt' cs (ds ++ fromEnum((toEnum c)+1))
>               in encrypt' cs fromEnum((toEnum c)+1)
>
> decrypt :: [Char] -> [Char]
> decrypt (c:cs) =
>               let decrypt' (c:cs) ds = 
>                       decrypt' cs (ds ++ fromEnum((toEnum c)-1))
>               in decrypt' cs fromEnum((toEnum c)-1)
>
