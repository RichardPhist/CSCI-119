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

--Grammar for number 5 from lab 11
--Original: S --> aSb | bSa | SS | ε
--in TNF:   S --> aSb | bSa | aSbS | bSaS | ab | ba | abS | baS
g5 :: CFG
g5 = ('S', d) where
  d 'S' 'a' = ["Sb", "SbS", "b", "bS"]
  d 'S' 'b' = ["Sa", "SaS", "a", "aS"]

--Grammar for number 6 from lab 11
--Original: S --> bS | Sa | aSb | ε
--in TNF:   S --> bS | b | a | aSb | ab | bSa | ba | aa | aSba | aba
g6 :: CFG
g6 = ('S', d) where
  d 'S' 'a' = ["Sb", "b" , "a", "Sba", "ba"]
  d 'S' 'b' = ["S", "a", "Sa"]

--Grammar for number 2 from lab 11
--Original: <RegExp> --> <Cat> | <RegExp> + <Cat>
--           <Cat> -->    <Star> | <Star> <Cat>
--           <Star> -->   <Atom> | <Atom> *
--           <Atom> -->   Σ | 1 | 0 | (<RegExp>)
--in TNF:   S -->   C | C + S
--          C -->   R | R C
--          R -->   A | A *
--          A -->   Σ | 1 | 0 | (S)
sigma = "abcdefghijklmnopqrstuvwxyz"
g2 :: CFG
g2 = ('S', d) where
  d 'S' '1' = ["","*","C","*C","+S","*+S","C+S","*C+S"]
  d 'S' '0' = ["","*","C","*C","+S","*+S","C+S","*C+S"]
  d 'S' '(' = ["S)","S)S","S)*","S)*S", "S)+S", "S)*+S", "S)C+S", "S)*C+S" ]
  d 'S' x
    | x `elem` sigma = ["","*","C","*C","+S","*+S","C+S","*C+S"]
    | otherwise = []

  d 'C' '1' = ["","*","C","*C"]
  d 'C' '0' = ["","*","C","*C"]
  d 'C' '(' = ["S)","S)*","S)C","S)*C"]
  d 'C' x
    | x `elem` sigma = ["","*","C","*C"]
    | otherwise = []

  d 'R' '1' = ["","*"]
  d 'R' '0' = ["","*"]
  d 'R' '(' = ["S)","S)*"]
  d 'R' x
    | x `elem` sigma = ["","*"]
    | otherwise = []
  
  d 'A' '1' = [""]
  d 'A' '0' = [""]
  d 'A' '(' = ["S)"]
  d 'A' x
    | x `elem` sigma = [""]
    | otherwise = []

----------------TESTS----------------
--accept g5 "abba" = True
--accept g5 "ab" = True
--accept g5 "abbbba" = False
--accept g5 "ba" = True
--accept g5 "aa" = False

--accept g6 "abba" = False
--accept g6 "ab" = True
--accept g6 "abbbba" = False
--accept g6 "ba" = True
--accept g6 "aa" = True

--sigma = "abcdefghijklmnopqrstuvwxyz"
--accept g2 sigma = True
--accept g2 "antidisestablishmentarianism" = True
--accept g2 "football+basketball" = True
--accept g2 "(ab*)+a*b+(b+ab+ba)*" = True
--accept g2 ")" = False
--accept g2 "(ab*+bbba*b+b*)ab+ba*)" = False