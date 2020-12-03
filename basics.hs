module Main where

-- | basic haskell function example
inRange :: Integer -> Integer -> Integer -> Bool   -- | function definition
inRange min max x = x >= min && x <= max           -- | function body

{-|
  inRange 0 5 3 -> will be True
  inRange 4 5 3 -> will be False
  inRange 1 1.5 1.2 -> will be a Type error, coz 2 arguments are Float, not Integer
-}

-- | Using vars
inRangeWithBody :: Integer -> Integer -> Integer -> Bool
inRangeWithBody min max x =
  let lowerBound = min <= x
      upperBound = max >= x
   in
  lowerBound && upperBound

inRangeWhere :: Integer -> Integer -> Integer -> Bool
inRangeWhere min max x = lb && ub
  where 
    lb = min <= x
    ub = max >= x

-- | Using conditions
inRangeIf min max x =
  if lb then ub else False
    where
      lb = min <= x
      ub = max >= x

{-|
  infix: operators are also functions in Haskell, for example, 
    definition of add infix function
  (+) :: Num a => a -> a -> a

  usage: 
    add a b = a + b
    add 10 20
    10 `add` 20
-}

-- | Recursion
factorial :: Integer -> Integer
factorial n =
  if n <= 1 then
    1
  else
    n * fac ( n - 1 )

-- | Guards
-- | factorialWithGuards :: ( Integer x ) => x -> Integer
factorialWithGuards n 
  | n <= 1    = 1
  | otherwise = n * factorialWithGuards ( n - 1 )

-- | Pattern Matching
isZero 0 = True
isZero _ = False -- | _ (wildcard) means that we match anything

-- | Accumulators - helps us to achieve tail recursion. 
-- Tail recursion enables us to write no stack memory using recursive functions (infinite).
fac n = aux n 1     -- | 1 is an accumulator
  where
    aux n acc
      | n <= 1    = acc
      | otherwise = aux ( n - 1 ) ( n * acc )

main :: IO ()
main = do
  putStrLn "Welcome to Haskell tutorial!"

