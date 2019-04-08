module RhythmGn.GenerativeGrammars where
import Euterpea
import Data.List hiding (transpose)
import System.Random

-- checks there is a single production for each Non-terminal symbol.
testDet :: Grammar a -> Bool
testDet g = error "Not implimented"

-- Generative framework for a generic grammar

data Grammar a = Grammar  a          --  start sentence
                          (Rules a)  --  production rules
     deriving Show

data Rules a  =  Uni  [Rule a] 
              |  Sto  [(Rule a, Prob)]
     deriving (Eq, Ord, Show)

data Rule a = Rule { lhs :: a, rhs :: a }
     deriving (Eq, Ord, Show)

type Prob = Double
type ReplFun a  = [[(Rule a, Prob)]] -> (a, [Rand]) -> (a, [Rand])
type Rand       = Double

gen :: Ord a => ReplFun a -> Grammar a -> Int -> [a]
gen f (Grammar s rules) seed = 
    let  Sto newRules  = toStoRules rules
         rands         = randomRs (0.0,1.0) (mkStdGen seed)
    in  if checkProbs newRules
        then generate f newRules (s,rands)
        else (error "Stochastic rule-set is malformed.")

toStoRules :: (Ord a, Eq a) => Rules a -> Rules a  
toStoRules (Sto rs)  = Sto rs
toStoRules (Uni rs)  = 
  let rs' = groupBy (\r1 r2 -> lhs r1 == lhs r2) (sort rs)
  in Sto (concatMap insertProb rs')

insertProb :: [a] -> [(a, Prob)] 
insertProb rules =  let prb = 1.0 / fromIntegral (length rules)
                    in zip rules (repeat prb)

checkProbs :: (Ord a, Eq a) => [(Rule a, Prob)] -> Bool
checkProbs rs = and (map checkSum (groupBy sameLHS (sort rs)))

eps = 0.001 

checkSum :: [(Rule a, Prob)] -> Bool 
checkSum rules =  let mySum = sum (map snd rules)
                  in abs (1.0 - mySum) <= eps 

sameLHS :: Eq a => (Rule a, Prob) -> (Rule a, Prob) -> Bool 
sameLHS (r1,f1) (r2,f2) = lhs r1 == lhs r2

generate ::  Eq a =>  
             ReplFun a -> [(Rule a, Prob)] -> (a,[Rand]) -> [a] 
generate f rules xs = 
  let  newRules      =  map probDist (groupBy sameLHS rules)
       probDist rrs  =  let (rs,ps) = unzip rrs
                        in zip rs (tail (scanl (+) 0 ps))
  in map fst (iterate (f newRules) xs)

-- L-System Grammar for Music
data LSys a  =  N a
             |  LSys a   :+   LSys a
             |  LSys a   :.   LSys a
             |  Id
     deriving (Eq, Ord, Show)

replFun :: Eq a => ReplFun (LSys a)
replFun rules (s, rands) =
  case s of
    a :+ b  ->  let  (a',rands')   = replFun rules (a, rands )
                     (b',rands'')  = replFun rules (b, rands')
                in (a' :+ b', rands'')
    a :. b  ->  let  (a',rands')   = replFun rules (a, rands )
                     (b',rands'')  = replFun rules (b, rands')
                in (a' :. b', rands'')
    Id      ->  (Id, rands)
    N x     ->  (getNewRHS rules (N x) (head rands), tail rands)

getNewRHS :: Eq a => [[(Rule a, Prob)]] -> a -> Rand -> a
getNewRHS rrs ls rand = 
  let  loop ((r,p):rs)  = if rand <= p then rhs r else loop rs
       loop []          = error "getNewRHS anomaly"
  in case (find (\ ((r,p):_) -> lhs r == ls) rrs) of
        Just rs  -> loop rs
        Nothing  -> error "No rule match"

-- Interpret the restulting sentences
type IR a b = [(a, Music b -> Music b)] -- <- interpretation rules go here

interpret :: (Eq a) => LSys a -> IR a b -> Music b -> Music b
interpret (a :. b)  r m = interpret a r (interpret b r m)  
interpret (a :+ b)  r m = interpret a r m :+: interpret b r m
interpret Id        r m = m 
interpret (N x)     r m = case (lookup x r) of
                            Just f   -> f m
                            Nothing  -> error "No interpetation rule"

-- interpretation rules
data LFun = Inc | Dec | Same
     deriving (Eq, Ord, Show)

ir :: IR LFun Pitch
ir = [ (Inc, transpose 1),
       (Dec, transpose (-1)),
       (Same, id)]

inc, dec, same :: LSys LFun
inc   = N Inc
dec   = N Dec
same  = N Same

-- define an actual grammar
sc = inc :+ dec

r1a  = Rule inc (sc :. sc)
r1b  = Rule inc sc
r2a  = Rule dec (sc :. sc)
r2b  = Rule dec sc
r3a  = Rule same inc
r3b  = Rule same dec
r3c  = Rule same same

g1 = Grammar same (Uni [r1b, r1a, r2b, r2a, r3a, r3b])

t1 n =  instrument RhodesPiano $
        interpret (gen replFun g1 42 !! n) ir (c 5 tn)

-- play (t1 3)


{--

            Instrument i
            
            interpret (LSys a) (IR ab) (Music b)

            ((gen replFun g1 42) !! n) ir (c 5 tn)

(!!) funtion takes a list (left) and an int (right) and returns the element at that index

            gen (ReplFun a) (Gramar a) (Int)

            replFun (LSys a)
--}

-- 12.3 change prodution rules, add probabilities to the roles; Sto grammar.
-- change the number seed, depth of recursion and musical seed.


-- 12.4 define a new version of LSys and its interpretation e.g. add a parallel
-- constructor 
-- or define a new version of LFun and its associated interpretation e.g. add
-- something to controle volume.
