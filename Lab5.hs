-- CSci 119, Lab 5
-- Reference: Lecture notes, Sections 4.1, 4.2

import Data.List (isInfixOf, isSuffixOf)  -- useful for testing in Part 2

-- Again, for this (and most) labs, the alphabet will be {a,b} to keep testing
-- easier, but your code should work just as well for any finite list.
sigma = ['a', 'b']

-- Finite State Machine M = (Q, s, F, d) with integer states
type FSM = ([Int], Int, [Int], Int -> Char -> Int)


---------------- Part 1: Representing FSMs

-- Check whether a finite state machine (qs, s, fs, d) is correct/complete:
-- (1) States qs are unique (no duplicates)
-- (2) Start state is a state (s is in qs)
-- (3) Final states are states (fs is a subset of qs)
-- (4) Transition function d gives a state in qs for every state in qs and
--     letter from sigma

unique :: [Int] -> Bool
unique [] = True
unique (q:qs) = if (and[x == z |x <- head [qs], y <- tail [qs], z <- head [y]] == False) then False else
  True 

subset :: [Int] -> Bool
subset empty = True
subset (fs:qs) = fs `elem` qs && subset qs

checkFSM :: FSM -> Bool
--checkFSM (qs, s, fs, d) = unique qs && (memb s qs) && subset fs qs
checkFSM (qs, s, fs, d) = nodups qs && s `elem` qs && all (`elem` qs) fs && func
  where nodups [] = True
        nodups (q:qs) = q `notElem` qs && nodups qs
        func = and [d q a `elem` qs | q <- qs, a <- sigma]

-- Gives the delta* function (recursive in w)
dstar :: FSM -> Int -> [Char] -> Int
dstar (_, _, _, d) = foldl d

-- Machine acceptance, Definition 1 (via delta*)
accept1 :: FSM -> [Char] -> Bool
accept1 m@(qs, s, fs, d) w = dstar m s w `elem` fs

-- Machine acceptance, Definition 2 (via L_q(M))
accept2 :: FSM -> [Char] -> Bool
accept2 (qs, s, fs, d) w = aux s w where
  -- aux q w = whether the machine, starting in q, accepts w (recursive in w)
  aux :: Int -> [Char] -> Bool
  aux q [] = q `elem` fs
  aux q (x:xs) = aux (d q x) xs


---------------- Part 2: FSM construction

-- Define a machine that accepts exactly the strings with an odd number of b's
-- (Solution given for illustration)
oddbs :: FSM
oddbs = ([0,1], 0, [1], d) where
  d q 'a' = q        -- stay in the same state
  d q 'b' = 1 - q    -- switch states

-- Define a machine that accepts exactly the strings that do not contain "aab"
-- as a substring and test it adequately
avoid_aab :: FSM
avoid_aab = ([0, 1, 2, 3] , 0, [0, 1, 2], d) where
  d q 'a' = if q <= 1 then q+1 else q
  d q 'b' = if q <= 1 then 0 else 3

-- Define a machine that accepts all strings that end in "ab" and test
end_ab :: FSM
end_ab = ([0,1,2], 0, [2], d) where
  d q 'a' = 1
  d q 'b' = if q == 1 then 2 else 0

-- Define a function that takes a string and returns a machine that accepts
-- exactly that string and nothing else (compare to 'onestr' from Lab 3)
-- Hint: the expression w ! i gives the i-th character of the string w
exactly :: String -> FSM
exactly w = ([0..n+1], 0, [n], d) where
  n = length w
  d i x = if i < n && w !! i == x then i+1 else n+1


-- Test the machines above. Also, try out the exerices at the end of Section 3.2
-- in my notes as FSMs. Do they produce the same results as your RegExp's did?

-- Recursive reimplementation of strings function from Lab 4
strings :: Int -> [String]
strings n = concat [strs i | i <- [0..n]] where
  strs 0 = [""]
  strs n = [a:xs | a <- sigma, xs <- strs (n-1)]

s10 = strings 10  -- or larger, if you like

oddbs_test1 = and [accept1 oddbs w == odd (num_bs w) | w <- s10] where
  num_bs w = sum (map (\x -> if x == 'b' then 1 else 0) w)

-------------------------TESTS-------------------------

--oddbs_test2 = and [accept2 oddbs w == odd (num_bs w) | w <- s10] where
--  num_bs w = sum (map (\x -> if x == 'b' then 1 else 0) w)

--unique [1,1,2] = True
--unique [1,2,3] = True

--oddbs_test1 = True
--oddbs_test2 = True
