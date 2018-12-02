import System.IO
import Control.Monad
import Debug.Trace
import Data.List

test1 = "abcdef"
test2 = "bababc"
test3 = "abbcde"
test4 = "abcccd"
test5 = "aabcdd"
test6 = "abcdee"
test7 = "ababab"

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

solvePartOne :: [String] -> Int
solvePartOne xs
  = x * y
  where
    (x, y) = solvePartOne' xs (0, 0)
    solvePartOne' :: [String] -> (Int, Int) -> (Int, Int)
    solvePartOne' [] (a, b)
      = (a, b)
    solvePartOne' (x : xs) (a, b)
      = solvePartOne' xs (a + countThem 2 x, b + countThem 3 x)
    countThem :: Int -> String -> Int
    countThem g a
      = if len >= 1 then 1 else 0
      where len  = length $ filter (\(_, x) -> x == g) $ nubBy (\(a1, _) (b1, _) -> a1 == b1)
                    (zip a
                      (map (\c -> count c a)
                      a))
  

main :: IO ()
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  let ls = lines contents
  putStr "Part one: "
  print $ solvePartOne ls
  -- putStr "Part two: "
  -- print $ solvePartTwo ls
