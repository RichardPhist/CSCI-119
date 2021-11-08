---- CSci 119, Lab 1 ----

-- Note: you will replace all instances of "undefined" below with your answers.


---- Boolean operators

-- The following code tests whether "and" is commutative:
bools = [True, False]
and_commutes = and [(p && q) == (q && p) | p <- bools, q <- bools]

-- Write similar defintions that test whether "or" is commutative,
-- "and" and "or" are associative, "and" distributes over "or",
-- "or" distributes over "and"
or_commutes = and [(p || q) == (q || p) | p <- bools, q <- bools]
and_assoc = and[((p && q) && r) == (p && (q && r)) | p <- bools, q <- bools, r <- bools]
or_assoc = and[[((p || q) || r) == (p || (q || r)) | p <- bools, q <- bools, r <- bools]
and_dist = and[((p && (q  || r)) == ((p && q) || (p && r))| p <- bools, q <- bools, r <- bools]
or_dist = and[((p || (q  && r)) == ((p || q) && (p || r))| p <- bools, q <- bools, r <- bools]
          
-- The exclusive-or operation on Bool in Haskell is equivalent to /=.
-- Test the properties of this operation (commutativity, associativity,
-- distributivity over and+or, and distributivity of and+or over it)
-- using the same method as above
xor_commutes = or[(p /= q) == (q /= p) | p <- bools, q <- bools]
xor_assoc    = or[[((p /= q) /= r) == (p /= (q /= r)) | p <- bools, q <- bools, r <- bools]
xor_dist_and = or[((p /= (q && r)) == ((p /= q) && (p /= r))| p <- bools, q <- bools, r <- bools]
xor_dist_or  = or[((p /= (q || r)) == ((p /= q) || (p /= r))| p <- bools, q <- bools, r <- bools]
and_dist_xor = or[((p && (q /= r)) == ((p && q) /= (p && r))| p <- bools, q <- bools, r <- bools]
or_dist_xor  = or[(p || (q /= r)) == ((p || q) /= (p || r)) | p <- bools, q <- bools, r <- bools]
               
-- The implication operator on Bool in Haskell is equivalent to <=.
-- Check whether implication is associative or commutative:
assoc_imp = [(p <= (q <= r)) == ((p <= q) <= r) | p <- bools, q <- bools, r <- bools]
comm_imp = [(p <= q) == (q <= p) | p <- bools, q <- bools]


----- Evaluating statements involving quantifiers

-- Assume that the universe of discourse is the set {1,2,3,4,5,6,7,8},
-- that is, that the word "number" temporarily means 1, 2, ..., 8.
-- Your solutions to the problems below should work no matter what
-- finite list of integers u is. For example, u = [5, 2, 17, 58, 21].

u = [1..8]

-- Translate each of the following statements first, in a comment, into a
-- logical statement involving forall, exists, and, or, imp, and not,
-- and then into Haskell code that checks ("brute force") whether
-- the statement is true. I'll work one example.

-- 1. "Every number that's greater than 2 is greater than 1"
-- A: forall n, (n > 2) imp (n > 1)
prob1_1 = and [(n > 2) <= (n > 1) | n <- u]   -- direct translation
prob1_2 = and [n > 1 | n <- u, n > 2]         -- using bounded quantifier

-- 2. Every number is either greater than 1 or less than 2
-- A: forall x, (x > 1) || (x < 2)
prob2 = and[(x > 1) || (x < 2) | x <- u] --direct translation

-- 3. Every two numbers are comparable with <= (i.e., either one is <=
--    the other or vice-versa)
-- A: forall x and y, (x <= y) or (y <= x)
prob3 = and[((x <= y) || (y <= x)) | x <- u, y <- u] --direct translation

-- 4. There is an odd number greater than 4
-- A: there exists an x (x > 4) && (odd x)
prob4 = or[(x > 4) && (odd x) | x <- u] --direct translation

-- 5: There are two odd numbers that add up to 10
-- A: there exists an x and y where (odd x && odd y) -> (x + y == 10)
prob5 = or[(odd x && odd y) <= (x + y == 10) | x <- u, y <- u]

-- 6. For every odd number, there is a greater even number (use the Haskell
--    predicates odd, even :: Integral a => a -> Bool)
-- A: for all x's that are odd there exists an even y that is greater than x, (odd x && even y) -> (x < y)
prob6 = and[or[((odd x && even y) <= (x < y))| x <- u, y <- u]] --direct translation

-- 7. For every even number, there is a greater odd number
-- A: for all x there exists a y where, (odd x && even y) -> (x > y)
prob7 = and[or[((odd x && even y) <= (y < x)) | x <- u, y <- u]]

-- 8. There are two odd numbers that add up to 6
-- A: there exists an x and a y where (x + y == 6) && (odd x && odd y)
prob8 = or[or[(x + y == 6) && (odd x && odd y) | x <- u, y <- u]]

-- 9. There is a number that is at least as large as every number
--    (i.e., according to >=)
-- A: for all x there exists a smaller y. There is a y that is greater than or equal to all x's
prob9 = and[or[(x <= y) | x <- u, y <- u]]

-- 10. For every number, there is a different number such that there are no
--    numbers between these two.
-- A: for all x there is a y where, (x-y == 1) || (y-x == 1) 
prob10 = and[or[((x - y == 1) || (y - x == 1)) | x <- u, y <- u]]
