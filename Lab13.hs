import Data.List (foldl')
import Data.Char (isUpper)

-- CFG G = (Start, Follows)
type CFG = (Char, Char -> Char -> [String])

accept :: CFG -> [Char] -> Bool
accept (s,d) = elem [] . foldl' (\xs c -> concatMap (lq c) xs) [[s]] where
  lq a [] = []
  lq a (x:xs) | isUpper x = map (++ xs) $ d x a          -- nonterminal
              | otherwise = if a == x then [xs] else []  -- terminal

-- Example 1: Balanced parentheses (not including the empty string)
-- original: S --> () | (S) | SS
-- in TNF:   S --> () | ()S | (S) | (S)S
bp :: CFG
bp = ('S', d) where
  d 'S' '(' = [")", ")S", "S)", "S)S"]
  d 'S' ')' = []

-- Example 2: {a^i b^j c^{i+j} | i >= 0, j > 0}
-- original: S --> aSc | T
--           T --> bTc | bc
-- in TNF:   S --> aSc | bTc | bc
--           T --> bTc | bc
pl = ('S', d) where
  d 'S' 'a' = ["Sc"]  ;  d 'S' 'b' = ["Tc","c"]  ;  d 'S' 'c' = []
  d 'T' 'a' = []      ;  d 'T' 'b' = ["Tc","c"]  ;  d 'T' 'c' = []

--------------------------LAB 13 BEGINS HERE--------------------------

-- CFG' G = (Start, Follows, Nullable)
type CFG' = (Char, Char -> Char -> [String], Char -> Bool)

accept' :: CFG' -> String -> Bool
accept' (s,f,n) = elem [] . foldl' (\xs c -> concatMap (lq c) xs) [[s]] where
  lq a [] = []
  lq a (x:xs) | isUpper x = map (++ xs) $ f x a          -- nonterminal
              | otherwise = if a == x then [xs] else []  -- terminal
  close :: CFG' -> String -> [String]
  close a [] = []
  close m@(s,f,n) w@(x:xs) | n x == True = [] ++ w ++ (close m xs)
                           | otherwise = [] ++ w


--------------------------TESTS--------------------------

-- Balanced parentheses
-- Original:  S --> (S) | SS | ε
-- TNF + ε:   S --> (S) | (S)S  (S nullable)

bp' :: CFG'
bp' = ('S', d, e) where
  d 'S' '(' = ["S)", "S)S"]
  d 'S' ')' = []
  e 'S' = True

--and[accept' g1 w = accept g2 w | w <- string 20]
--Outputs: 

--Grammar for number 5 from lab 11
--Original: S --> aSb | bSa | SS | ε
--in TNF:   S --> aSb | bSa | aSbS | bSaS | ab | bs | abS | baS
g5 :: CFG
g5 = ('S', d) where
  d 'S' 'a' = ["Sb", "SbS", "b", "bS"]
  d 'S' 'b' = ["Sa", "SaS", "a", "aS"]

g5asCFG' :: CFG'
g5asCFG' = ('S', d, n) where
  d 'S' 'a' = ["", "Sb", "S"]
  d 'S' 'b' = ["", "Sa", "S"]
  n 'S' = True

