> module Chapters.Ch12 where
> import Euterpea
> import Data.List hiding (transpose)
> import System.Random

key terms:
 grammar describes a formal language
 designed either as a recogoniser (parser) or a generator
 generative grammars are the latter
 N is a set of non-terminals, T is a set of terminals, n is the starting
 point, P is a set of production rules
 L-system is an example of generative grammar where sequence of sentences are
 as important as the individual sentesnces
 A new sentence is generated from the previous one by applying as many
 productions as possible on each step
 Contex-free grammars are where the left-hand side of each production is a
 single non-terminal
 deterministic grammars have exactly one production corresponding to each
 non-terminal, while non-deterministic grammars have more than one and must
 decide how to choose between them.


framework for Deterministic Grammar
remember P is (X,Y) where Y is N U T & X is a single non-terminal
(context-free)

> data DetGrammar a = DetGrammar a            -- start symbol
>                                [(a,[a])]    -- productions
>   deriving Show

given a grammar, return a list of symbols (all possible productions?)

> detGenerate :: Eq a => DetGrammar a -> [[a]]
> detGenerate (DetGrammar st ps) = iterate (concatMap f) [st]
>   where f a = maybe [a] id (lookup a ps)

example grammar

> redAlgae = DetGrammar 'a'
>                [  ('a',"b|c"),   ('b',"b"),  ('c',"b|d"),
>                   ('d',"e\\d"),  ('e',"f"),  ('f',"g"),
>                   ('g',"h(a)"),  ('h',"h"),  ('|',"|"),
>                   ('(',"("),     (')',")"),  ('/',"\\"),
>                   ('\\',"/")
>                ]

pretty print

> t n g = sequence_ (map putStrLn (take n (detGenerate g)))

turn a production into music

> -- getLastDetGrammar :: DetGrammar a -> Int -> String
> -- getLastDetGrammar g n = last (take n (detGenerate g))

Exercise 12.1

 strToMusic::AbsPitch -> Dur -> String -> Maybe (Music Pitch)
 strToMusic ap d str = case f x of 
                         Nothing -> True
                         Just x -> line (map f str)
                 where f x | x == 'a' = ap d
                           | x == 'b' = note d (ap + 2)
                           | x == 'c' = note d (ap + 4)
                           | x == 'd' = note d (ap + 5)
                           | x == 'e' = note d (ap + 7)
                           | x == 'f' = note d (ap + 9)
                           | x == 'g' = note d (ap + 11)
                           | x == 'h' = note d (ap + 12)
                           | x == '|' = Nothing
                           | x == '/' = rest d
                           | x == '\\' = rest d
                           -- | x == '(' = transpose 5 (note d (pitch ap))
                           -- | x == ')' = transpose (-5) (note d (pitch ap))


Exercise 12.2

> testDet :: DetGrammar a -> Bool
> testDet g = error "Not implimented"

