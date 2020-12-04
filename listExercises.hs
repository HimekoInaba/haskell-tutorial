import System.Environment
import Data.List

-- | Execise 1. Create a function kekelem that returns True if an kekelement is in a given list and returns False otherwise
kekelem :: (Eq a) => a -> [a] -> Bool
kekelem _ [] = False
kekelem a (x:xs) = (a == x) || (kekelem a xs)

nubkek :: (Eq a) => [a] -> [a]
nubkek [] = []
nubkek (x:xs)
  | x `kekelem` xs = nubkek xs
  | otherwise      = x : nubkek xs

isAsc :: [Int] -> Bool
isAsc []  = True
isAsc [_] = True
isAsc (x:y:xs)
  | x <= y = isAsc $ y:xs
  | otherwise = False

hasPath :: [(Int,Int)] -> Int -> Int -> Bool
hasPath [] v b  = v == b
hasPath ((a,b):xs) v u = ( (a == v) && hasPath xs b u ) || ( (b == u) && hasPath xs a v ) || hasPath xs v u 

main :: IO ()
main = do
  args <- getArgs
  let n = head args
  let xs = tail args
  print ( hasPath [(1,2), (2,3), (3,2), (4,3), (4,5)] 4 2)
