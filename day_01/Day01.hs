import System.IO
import Control.Monad
import Debug.Trace

solvePartOne :: [Int] -> Int
solvePartOne = foldl (+) 0


solvePartTwo :: [Int] -> Int
solvePartTwo as = solvePartTwo' [] 0 (cycle as)
  where
    solvePartTwo' :: [Int] -> Int -> [Int] -> Int
    solvePartTwo' fs freq (x : xs)
      = case elem freq fs of
          True -> freq
          False -> solvePartTwo' (freq : fs) (freq + x) xs

main :: IO ()
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  let ls = map (\x -> (read x) :: Int)
        $ map (dropWhile (== '+'))
        $ lines contents
  putStr "Part one: "
  print $ solvePartOne ls
  putStr "Part two: "
  print $ solvePartTwo ls
