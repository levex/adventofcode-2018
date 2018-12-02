import System.IO
import Control.Monad
import Debug.Trace
import Data.List
import qualified Data.MemoCombinators as Memo

test1 = "abcdef"
test2 = "bababc"
test3 = "abbcde"
test4 = "abcccd"
test5 = "aabcdd"
test6 = "abcdee"
test7 = "ababab"


test8 = ["bcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

memoString = Memo.list Memo.char

lev' :: String -> String -> Int
lev' xs ys = levMemo xs ys
  where levMemo = Memo.memo2 memoString memoString lev
        lev [] [] = 0
        lev [] ys = length ys
        lev xs [] = length xs
        lev (x:xs) (y:ys)
          | x == y    = levMemo xs ys
          | otherwise = 1 + minimum [levMemo xs (y:ys),
                                     levMemo (x:xs) ys,
                                     levMemo xs ys]

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
  
-- solvePartTwo :: [String] -> (String, String)
solvePartTwo xs
  = (\(a, b, _) -> (a, b)) $ head $ filter (\(a, b, dist) -> dist == 1) distances
  where
    distances = map (\(a, b) -> (a, b, lev' a b)) all
    all = zip (concat $ map (\x -> take (length xs) $ repeat x) xs) (cycle xs)

main :: IO ()
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  let ls = lines contents
  putStr "Part one: "
  print $ solvePartOne ls
  putStr "Part two: "
  print $ solvePartTwo ls
