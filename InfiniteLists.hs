module InfiniteLists where

-- | construct a name generator
--
-- names = ["a".."z", "a1".."z1", "a2".."z2", ...]

names :: [String]
names = [ [y] ++ toString x | x <- [0..], y <- ['a'..'z'] ]
  where toString n = if n == 0 then "" else show n


-- | constructs the infinite sequence
-- of fibonacci numbers in linear time
--
-- fibs = [0, 1, 1, 2, 3, 5, 8, ...]

fibs :: [Integer]
fibs = 0:1:zipWith (+) (tail fibs) fibs

-- ----------------------------------------
--
-- | naive prime number generator with
-- sieve of Eratosthenes and a recursive
-- sieve operation

primes :: [Integer]
primes = sieve [2..]
  where sieve (x:xs) = x : sieve [ y | y <- xs, mod y x /= 0 ]

-- ----------------------------------------
--
-- | the hamiltonian sequence is the ordered sequence
-- of natural number which are multiples of 2 or 3 or 5
--
-- Implementation: the 3 lists of multiples of 2, 3 and 5
-- are merged together with a @merges@ function.
--
-- The direct solution is

hamilton' :: [Integer]
hamilton'
  = filter
    (\ i ->    i `mod` 2 == 0
            || i `mod` 3 == 0
            || i `mod` 5 == 0
    ) [0..]

-- | @hamilton@ by merging sequences

hamilton :: [Integer]
hamilton
  = merges [is2, is3, is5]
    where
      is2 = [ x | x <- [2..], mod x 2 == 0 ]
      is3 = [ y | y <- [3..], mod y 3 == 0 ]
      is5 = [ z | z <- [5..], mod z 5 == 0 ]

merge :: [Integer] -> [Integer] -> [Integer]
merge [] [] = []
merge [] _  = []
merge _  [] = []
merge (x:xs) (y:ys)
  | x < y  = x:y:merge xs ys
  | x > y  = y:x:merge xs ys
  | x == y = x:merge xs ys

-- | @merges@ takes a list of lists of ascending integers
-- and merges these lists into a single sorted list without any duplicates
-- direct impl

merges :: [[Integer]] -> [Integer]
merges [] = []
merges (ys:xs) = merge ys $ merges xs

-- | @merges@ with a fold

merges' :: [[Integer]] -> [Integer]
merges' = undefined    -- after chapter about folds

-- ----------------------------------------
