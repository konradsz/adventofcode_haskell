import Data.Char (digitToInt)
import Data.List (group)
import System.Environment (getArgs)

main :: IO ()
main = do
  (input : _) <- getArgs
  input <- readFile input
  let initial = map digitToInt input
  print $ part1 initial
  print $ part2 initial

part1 :: [Int] -> Int
part1 input = length $ grow 40 input

part2 :: [Int] -> Int
part2 input = length $ grow 50 input

process :: [Int] -> [Int]
process input = concatMap (\x -> [length x, head x]) $ group input

grow :: Int -> [Int] -> [Int]
grow n l = iterate process l !! n