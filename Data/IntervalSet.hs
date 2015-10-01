module Data.IntervalSet
where

-- ----------------------------------------

-- an pair of Ints can represent closed Intervals
-- (i, j) <=> [i..j]
-- Intervalls with i > j represent the empty set

type Interval = (Int, Int)

overlap :: Interval -> Interval -> Bool
overlap (x1, y1) (x2, y2)
  | x1 > x2   = overlap (x2, y2) (x1, y1)
  | otherwise = y1 + 1 >= x2
                        

less :: Interval -> Interval -> Bool
less (_x1, y1) (x2, _y2)
  = y1 < x2

                           
emptyInterval :: Interval -> Bool
emptyInterval (x, y)
  = x > y


-- merge 2 (overlapping) intervals
merge :: Interval -> Interval -> Interval
merge (x1, y1) (x2, y2) = (min x1 x2, max y1 y2)


-- ----------------------------------------

-- a set of integers can be represented by an
-- ordered list of none empty intervals, which
-- do not overlap

type IntervalSet = [Interval]

inv :: IntervalSet -> Bool
inv xs = and $ zipWith less xs (tail xs)


-- ----------------------------------------
-- internal interval set ops

singleInterval :: Int -> Int -> IntervalSet
singleInterval x y
    | x <= y    = [(x, y)]
    | otherwise = []

insertInterval :: Interval -> IntervalSet -> IntervalSet
insertInterval x [] = [x]
insertInterval x (next:xs)
  | overlap x next = merge x next : xs
  | less x next = x : next : xs
  | otherwise = next : insertInterval x xs
                

fromIntervalList :: [(Int, Int)] -> IntervalSet
fromIntervalList = foldr insertInterval []

-- ----------------------------------------
--
-- exported ops, names similar to Data.Set

empty :: IntervalSet
empty = []

singleton :: Int -> IntervalSet
singleton i = singleInterval i i

insert :: Int -> IntervalSet -> IntervalSet
insert i = insertInterval (i, i)

union :: IntervalSet -> IntervalSet -> IntervalSet
union = foldr insertInterval


member :: Int -> IntervalSet -> Bool
member i = or . map isIn
  where isIn (x, y) = x < i && i < y

         
fromList :: [Int] -> IntervalSet
fromList = foldr insert empty


toList :: IntervalSet -> [Int]
toList = concat . map (uncurry upto)
  where upto x y = [x..y]


-- ----------------------------------------
