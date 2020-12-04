-- | import functions on lists
import Data.List

{-|
   Lists - fixed index, can hold only 1 type
   [ 1, 2, 3, 4, 5] :: [Integer]
   
   Creation: [] - constructor, x:xs - prepend: add x to list xs, example: 1 : 2 : 3 : 4 : 5 : []
-}

-- | Creates ascendind list from n to m. Example: asc 1 3 => [1, 2, 3]
asc :: Int -> Int -> [Int]
asc n m
  | m < n = []
  | m == n = [m]
  | m > n = n : asc ( n + 1 ) m

{-|
   *** Frequently used prebuilt list functions ***

   head   :: [a]    -> a    - returns first element of list
   tail   :: [a]    -> [a]  - returns list without first element
   length :: [a]    -> Int  - returns length of list
   init   :: [a]    -> [a]  - return list without last element
   null   :: [a]    -> Bool - list is empty?
   and    :: [Bool] -> Bool - apply logical and on all list elements
   or     :: [Bool] -> Bool - apply logical or on all list elements
-}

{-|
   *** List comprehension ***

   [ 2 * x | x <- [1, 2, 3] ] => [2, 4, 6]
   [ 2 * x | x <- [1, 2, 3], x > 1] => [4, 6]
   [ ( x, y ) | x <- [1, 2, 3], y <- ['a', 'b'] ] => [ (1, 'a'), (1, 'b'), (2, 'a'), (2, 'b'), (3, 'a'), (3, 'b') ]

   [ <gen> | <elem> <- <list>, ..., <guard>, ... ]
-}

concatIntAndCharArraysToStringArray :: [Int] -> [Char] -> [String]
concatIntAndCharArraysToStringArray ints chars = [ ( show x ) ++ [y] | x <- ints, y <- chars]

-- | List patterns
sumList :: [Int] -> Int
sumList []         = 0
sumList ( x : xs ) = x + sum xs

evens :: [Int] -> [Int]
evens []         = []
evens ( x : xs )
  | mod x 2 == 0 = x : evens  xs
  | otherwise    = evens xs

{-| 
   *** Tuples ***
   
   (1, 2.3) :: (Int, Float) - can hold different types
   let ( x, y ) = (1, 2) in x => 1
-}

getFirstTuple :: ( a, b ) -> a
getFirstTuple ( x, _ ) = x

getSecondTuple :: ( a, b ) -> b
getSecondTuple ( _, y) = y

-- | addTuples [ (1, 2), (2, 3), (100 , 100) ] => [ 3, 5, 200 ]
addTuples :: [(Int, Int)] -> [Int]
addTuples xs = [ x + y | ( x, y ) <- xs ]

main :: IO ()
main = do
  print ( concatIntAndCharArraysToStringArray [1, 2, 3, 4, 5] ['a', 'b', 'c', 'd', 'e'] )

