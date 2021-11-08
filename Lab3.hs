-- CSci 119, Lab 3

-- See https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html
import Data.List (sort, stripPrefix)


---------------- General list functions

-- Normalize a list: sort and remove duplicates
norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys

-- Cartesian product, preserves normalization
cart :: [a] -> [b] -> [(a,b)]
cart xs ys = [(a,b) | a <- xs, b <- ys]

-- Powerset, preserves normalization. Examples:
-- power [] = [[]]
-- power [1] = [[],[1]]
-- power [1,2] = [[],[1],[1,2],[2]]
-- power [1,2,3] = [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]
-- power [2,3] = [[],[2],[2,3],3]]
power :: [a] -> [[a]]
power [] = [[]]
power (x:xs) = let ys = power xs in [] : map (x:) ys ++ tail ys

---------------- Length-ordered lists

-- Length-Ordered Lists over "character type" a (aka "strings over a")
-- Invariant: In LOL n xs, n == length xs
-- Note that automatically-derived Ord instance correctly orders LOLs
data LOL a = LOL Int [a] deriving (Eq,Ord)

instance Show a => Show (LOL a) where
  show (LOL n xs) = show xs

-- Empty list (epsilon)
eps :: LOL a
eps = LOL 0 []

-- Smart constructor for LOL a, establishes invariant
lol :: [a] -> LOL a
lol xs = LOL (length xs) xs

-- Concatenation of LOLs, preserves invariant
dot :: LOL a -> LOL a -> LOL a
dot (LOL n xs) (LOL m ys) = LOL (n + m) (xs ++ ys)

-- Reverse of LOLs, preserves invariant
rev :: LOL a -> LOL a
--rev [] = []
rev (LOL b (x:xs)) = (LOL b (reverse xs ++ [x]))



---------------- Languages

-- Normalized lists of LOLs (aka "languages")
-- Invariant: xs :: Lang a implies xs is ordered with no duplicates
type Lang a = [LOL a]


-- Constructor for languages, establishes invariant
lang :: Ord a => [[a]] -> Lang a
lang xs = norm $ map lol xs

-- Membership for languages (infinite lists satisfying invariant included)
memb :: Ord a => LOL a -> Lang a -> Bool
memb _ [] = False
memb x (y:ys) = case compare x y of 
  LT -> False
  EQ -> True
  GT -> memb x ys

-- Merge of langages (aka "union")
merge :: Ord a => Lang a -> Lang a -> Lang a
merge [] ys = ys
merge xs [] = xs
merge xs@(x:xr) ys@(y:yr) =
  case compare x y of
    LT -> x : merge xr ys
    EQ -> x : merge xr yr
    GT -> y : merge xs yr

-- Concatenation of languages
cat :: Ord a => Lang a -> Lang a -> Lang a
cat [] xs = xs
cat ys [] = ys
cat (x:xs) ys = merge [dot x y | y <- ys] (cat xs ys)

-- Kleene star of languages
kstar :: Ord a => Lang a -> Lang a
kstar [] = [eps]
kstar (LOL 0 []:xr) = kstar xr 
kstar xs = eps : cat xs (kstar xs)

-- Left quotient of a language by an LOL (cf. Definition 2.16)
-- Hint: Use the stripPrefix function
leftq :: Ord a => LOL a -> Lang a -> Lang a
leftq _ [] = []
leftq w@(LOL n xs) (LOL m ys:yss)
  | n <= m = case stripPrefix xs ys of 
      Nothing -> leftq w yss
      Just zs -> LOL (m-n) zs : leftq w yss
  | otherwise = leftq w yss


---- Regular expressions and the languages they denote 
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


-- The language associated to a regular expression, i.e., [[r]]
lang_of :: RegExp -> Lang Char
lang_of Empty = []
lang_of (Let r) = lang [[r]]
lang_of (Union r1 r2) = merge (lang_of r1) (lang_of r2) 
lang_of (Cat r1 r2) = cat (lang_of r1) (lang_of r2) 
lang_of (Star r) = kstar (lang_of r)

-- The one-string and finite languages of Theorem 3.2. It should be the case
-- that, for any string w, lang_of (onestr w) == [w], and for any (finite) list
-- of (distinct, sorted) strings l, lang_of (finite l) == l.
onestr :: String -> RegExp
onestr [] = Star Empty
onestr [x] = Let x
onestr (x:xs) = Cat (Let x) (onestr xs)

finite :: [String] -> RegExp
finite [] = Empty
finite [w] = onestr w
finite (w:ws) = Union (onestr w) (finite ws)


-- Test all of the above operations extensively! 

--langages used for testing
test = lang ["pee", "poo"]
test2 = lang ["soup", "burger"]
test3 = lang [""]
test4 = lang ["pee","poo","souppee","souppoo","burgerpee","burgerpoo"]
list = ["mouse", "avocado", "watermelon"]

--merge test test2
--Outputs: ["pee","poo","soup","burger"]

--memb (lol "ab") (test)
--Outputs: False
--memb (lol "ab") test2
--Outputs: False
--memb (lol "poo") (test)
--OutPut: True

--cat test test2
--Outputs: ["soup","burger","peesoup","poosoup","peeburger","pooburger"]
--cat test2 test3
--Outputs: ["soup","burger"]

--kstar test
--Outputs: ["pee", "poo", "peepoo" ...]

--rev (lol "racecar")
--Outputs: exactly what you think it would "racecar"

--rev (lol "palindrome")
--Outputs: "emordnilap"

--rev (lol "antidisestablishmentarianism")
--Outputs: "msinairatnemhsilbatsesiditna"

--left q (lol "soup") test4
--Outputs: ["pee", "poo]

--leftq (lol "pee") test4
--Outputs: [""]

--onestr "bucket"
--Outputs: Cat (Let 'b') (Cat (Let 'u') (Cat (Let 'c') (Cat (Let 'k') (Cat (Let 'e') (Let 't')))))

--finite list
--Outputs: Union (Cat (Let 'm') (Cat (Let 'o') (Cat (Let 'u') (Cat (Let 's') (Let 'e'))))) (Union (Cat (Let 'a') (Cat (Let 'v') (Cat (Let 'o') (Cat (Let 'c') (Cat (Let 'a') (Cat (Let 'd') (Let 'o'))))))) (Cat (Let 'w') (Cat (Let 'a') (Cat (Let 't') (Cat (Let 'e') (Cat (Let 'r') (Cat (Let 'm') (Cat (Let 'e') (Cat (Let 'l') (Cat (Let 'o') (Let 'n')))))))))))

--toRE "a*"
--Outputs: Star (Let 'a')

--lang_of (Star (Let 'a'))
--Outputs: "a", "a", "a", "a"...



