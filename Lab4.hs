-- CSci 119, Lab 4

import Data.List (sort, stripPrefix) -- for your solution to Lab 3
import Control.Monad (replicateM)    -- for strings function at the end


---------------- Code provided to you in Lab 3 ----------------

-- Normalize a list: sort and remove duplicates
norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys

-- Length-Ordered Lists over "character type" a (aka "strings")
-- Invariant: In LOL n xs, n == length xs
data LOL a = LOL Int [a] deriving (Eq,Ord)

instance Show a => Show (LOL a) where
  show (LOL n xs) = show xs

-- Empty list (epsilon)
eps :: LOL a
eps = LOL 0 []

-- Smart constructor for LOL a, establishes invariant
lol :: [a] -> LOL a
lol xs = LOL (length xs) xs

-- Normalized lists of LOLs (aka "languages")
-- Invariant: xs :: Lang a implies xs is sorted with no duplicates
type Lang a = [LOL a]

-- Smart constructor for (finite) languages
lang :: Ord a => [[a]] -> Lang a
lang xs = norm $ map lol xs

---- Regular expressions, along with input and output
data RegExp = Empty                -- Empty language
            | Let Char             -- Single letter language
            | Union RegExp RegExp  -- Union
            | Cat RegExp RegExp    -- Concatenation
            | Star RegExp          -- Kleene star
            deriving (Show, Eq)

-- Compact display form for RegExp
newtype Compact = Compact RegExp

instance (Show Compact) where    -- use precedence to minimize parentheses
  showsPrec d (Compact r) = sp d r where
    sp d Empty         = showString "@"
    sp d (Let c)       = showString [c]
    sp d (Union r1 r2) = showParen (d > 6) $  -- prec(Union) = 6
                         sp 6 r1 .
                         showString "+" .
                         sp 6 r2
    sp d (Cat r1 r2)   = showParen (d > 7) $  -- prec(Cat) = 7
                         sp 7 r1 .
                         sp 7 r2
    sp d (Star r1)     = sp 9 r1 .     -- prec(Star) = 8
                         showString "*"

-- Quick and dirty postfix RegExp parser, gives non-exaustive match on error
toRE :: String -> RegExp
toRE w = go w [] where
  go [] [r]              = r
  go ('+':xs) (r2:r1:rs) = go xs (Union r1 r2:rs)
  go ('.':xs) (r2:r1:rs) = go xs (Cat r1 r2:rs)
  go ('*':xs) (r:rs)     = go xs (Star r:rs)
  go ('@':xs) rs         = go xs (Empty:rs)
  go (x:xs) rs           = go xs (Let x:rs)


---------------- Your solution to Lab 3 ----------------

-- Include here any code you need from your solution to Lab 3 for testing
-- purposes. After a few days, I will release my solution for you to use.
-- Don't duplicate the code just given above.


---------------- Part 1 ----------------

-- Implement the seven recursive predicates/operations on RegExp given in
-- Section 3.3 of the notes; call them empty, unitary, byp, inf, rever, lq,
-- and nep. Each should begin with a type declaration. Include several tests
-- for each function.
empty :: RegExp -> Bool
empty Empty = True
empty (Let a) = False
empty (Union r1 r2) = empty r1 && empty r2
empty (Cat r1 r2) = empty r1 || empty r2
empty (Star r) = False

unitary :: RegExp -> Bool
unitary Empty = False
unitary (Let a) = False
unitary (Union r1 r2) = unitary r1 && empty r2 || empty r1 && unitary r2  || unitary r1 && unitary r2
unitary (Cat r1 r2) = unitary r1 && unitary r2
unitary (Star r1) = empty r1 || unitary r1

byp :: RegExp -> Bool
byp (Empty) = False
byp (Let a) = False
byp (Union r1 r2) = byp r1 || byp r2
byp (Cat r1 r2) = byp r1 && byp r2
byp (Star r) = True

inf :: RegExp -> Bool
inf Empty = False
inf (Let a) = False
inf (Union r1 r2) = inf r1 || inf r2
inf (Cat r1 r2) = (inf r1 && not(empty r2)) || (inf r2 && not (empty r1))
inf (Star r) = not(empty r) && not(unitary r)

rever :: RegExp -> RegExp
rever Empty = Empty
rever (Let a) = Let a
rever (Union r1 r2) = (Union (rever r2) (rever r1))
rever (Cat r1 r2) = (Cat (rever r2) (rever r1))
rever (Star r) = Star (rever r)

lq :: Char -> RegExp -> RegExp
lq w Empty = Empty
lq w (Let a) = if w == a then (Star Empty) else Empty
lq w (Union r1 r2) = (Union (lq w r1) (lq w r2))
lq w (Cat r1 r2) = if (byp r1) then (Union (Cat (lq w r1)  r2) (lq w r2)) else
  (Cat (lq w r1) r2)
lq w (Star r) = Cat (lq w r) (Star r)

nep :: RegExp -> RegExp
nep Empty = Empty
nep (a) = a
nep (Union r1 r2) = (Union (nep r1) (nep r2))
nep (Cat r1 r2) = if (byp r1) then (Union (Cat (nep r1) r2) (nep r2)) else
  (Cat (nep r1) (r2))
nep (Star r1) = (Cat (nep r1) (Star r1))


---------------- Part 2 ----------------

-- Implement the two matching algorithms given in Section 3.4 of the notes,
-- where the second algorithm is the modified one I posted on Piazza (@96).
-- Call them match1 and match2. Start by implementing splits:

-- splits xs = list of all possible splits of xs, in order. For example,
-- splits "abc" = [("","abc"), ("a","bc"), ("ab","c"), ("abc","")]
splits :: [a] -> [([a], [a])]
splits xs = [(take x xs, drop x xs) | x <- [0..(length xs)]]

match1 :: RegExp -> String -> Bool
match1 (Empty) w = False
match1 (Let a) w = (w == [a])
match1 (Union r1 r2) w = (match1 r1 w) || (match1 r2 w)
match1 (Cat r1 r2) w = or [match1 r1 w1 && match1 r2 w2 | (w1, w2) <- splits w]
match1 (Star r1) w = (w == "") || or [ w1 /= "" && (match1 r1 w1)
  && (match1 (Star r1) w2) | (w1, w2) <- splits w]

match2 :: RegExp -> String -> Bool
match2 r w = m [r] w where
  m:: [RegExp] -> String -> Bool
  m [] w = w == ""
  m ((Empty) : rs) w = False
  m ((Let a) : rs) w = False
  --m ((Let a) : rs) "" = False
  --m ((Let a) : rs) (b:w) = (a:w) == (b:w) && (m rs w)
  m ((Union r1 r2) : rs) w = (m (r1:rs) w) || (m (r2:rs) w)
  m (Cat r1 r2 : rs) w = (m (r1:r2:rs) w) || (length [w] == 0) && (byp r1) && (m (r2:rs) w)
  m (Star r1 : rs) w = ((length [w] == 1) && (m rs w)) || (m (r1:rs) w)

--match2 :: [RegExp] -> String -> Bool -> Bool
--match2 [] w c = w == ""
--match2 ((Empty):rs) w c = False
--match2 ((Let a):rs) "" c = False
--match2 ((Let a):rs) (b:w) c = (a:w) == (b:w) && (match2 rs w False)
--match2 ((Union r1 r2):rs) w c = (match2 (r1:rs) w c) || (match2 (r2:rs) w c)
--match2 ((Cat r1 r2):rs) w c = (match2 (r1:r2:rs) w c)
--                            --|| ( c == True && byp(r1) && (match2 (r2:rs) w True) )
--match2 ((Star r):rs) w c = (c == False && (match2 rs w False)) || (match2 (r:rs) w True)


-- Some regular expressions for testing. Also, test them on other solutions
-- to the exercises of Section 3.2 (as many as you can get). 

sigma = ['a', 'b']                -- Alphabet used in all examples below

ab   = toRE "aa.bb.+*"            -- every letter is duplicated
ttla = toRE "ab+*a.ab+.ab+."      -- third to last letter is a
ena  = toRE "b*a.b*.a.*b*."       -- even number of a's
bb1  = toRE "aba.+*b.b.aab.+*."   -- contains bb exactly once


-- For your tests, you may also find the following helpful. For example,
-- you can generate all strings of length 10 (or 20) or less and then test
-- to see whether match1 r w == memb (lol w) (lang_of r) for each such w.

-- All strings on sigma of length <= n (useful for testing)
strings :: Int -> [String]
strings n = concat $ [replicateM i sigma | i <- [0..n]]

------------------------------TESTS------------------------------

--empty (Star (Union (Let 'a') (Let 'b'))) = False
--empty (Star (Union (Empty) (Let 'b'))) = False
--empty (Star (Cat (Empty) (Let 'b'))) = False
--empty (Star (Cat (Let 'a') (Let 'b'))) = False
--empty (Empty) = True

--unitary (Star (Union (Let 'a') (Let 'b'))) = False
--unitary (Star (Cat (Let 'a') (Let 'b'))) = False
--unitary (Cat (Union (Let 'a') (Let 'b')) (Union (Let 'a') (Let 'a'))) = False 
--unitary (Union (Let 'a') (Empty)) = False
--unitary (Union (Empty) (Empty)) = True

--byp (Star (Union (Let 'a') (Let 'b'))) = True
--byp (Cat (Union (Let 'a') (Let 'b')) (Union (Let 'a') (Let 'a'))) = False

--inf (Star (Union (Let 'a') (Let 'b'))) = True
--inf (Cat (Union (Let 'a') (Let 'b')) (Union (Let 'a') (Let 'a'))) = False

--rever (Star (Union (Let 'a') (Let 'b'))) = Star (Union (Let 'b') (Let 'a'))
--rever (Cat (Union (Let 'a') (Let 'b')) (Union (Let 'a') (Let 'a'))) = Cat (Union (Let 'a') (Let 'a')) (Union (Let 'b') (Let 'a'))

--lq 'a' (Star (Union (Let 'a') (Let 'b'))) = Cat (Union (Star Empty) Empty) (Star (Union (Let 'a') (Let 'b')))
--lq 'b' (Star (Union (Let 'a') (Let 'b'))) = Cat (Union Empty (Star Empty)) (Star (Union (Let 'a') (Let 'b')))

--lq 'a' (Star (Cat (Let 'a') (Let 'b'))) = Cat (Cat (Star Empty) (Let 'b')) (Star (Cat (Let 'a') (Let 'b')))
--lq 'b' (Star (Cat (Let 'a') (Let 'b'))) = Cat (Cat Empty (Let 'b')) (Star (Cat (Let 'a') (Let 'b')))

--nep (Star (Union (Let 'a') (Let 'b'))) = Star (Union (Let 'a') (Let 'b'))
--nep (Union (Star (Let 'b')) (Cat (Let 'a') (Let 'b'))) = Union (Star (Let 'b')) (Cat (Let 'a') (Let 'b'))

--match1 (Star (Union (Let 'a') (Let 'b'))) "ab" = True
--match1 (Star (Union (Let 'a') (Let 'b'))) "aba" = True
--match1 (Star (Union (Let 'a') (Let 'b'))) "baba" = True
--match1 (Star (Union (Let 'a') (Let 'b'))) "aaaaaaab" = True
--match1 (Star (Union (Let 'a') (Let 'b'))) "bbbbbbba" = True

--match1 (Star (Cat (Let 'a') (Let 'b'))) "aba" = False
--match1 (Star (Cat (Let 'a') (Let 'b'))) "abab" = True
--match1 (Star (Cat (Let 'a') (Let 'b'))) "aaaaaaab" = False

--match2 (Star (Cat (Let 'a') (Let 'b'))) "ab" = False
--match2 (Star (Cat (Let 'a') (Let 'b'))) "abab" = False
--match2 (Star (Cat (Let 'a') (Let 'b'))) "aaaaaab" = False

--match2 (Star (Union (Let 'a') (Let 'b'))) "ab" = False
--match2 (Star (Union (Let 'a') (Let 'b'))) "abab" = False
--match2 (Star (Union (Let 'a') (Let 'b'))) "aaaaaab" = False


