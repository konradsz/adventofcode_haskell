import Data.List (group)

main :: IO ()
main = do
  let input = [1, 3, 2, 1, 1, 3, 1, 1, 1, 2]
  print $ part1 input
  print $ part2 input

part1 :: [Int] -> Int
part1 input = length $ grow 40 input

part2 :: [Int] -> Int
part2 input = length $ grow 50 input

process :: [Int] -> [Int]
process input = concatMap (\x -> [length x, head x]) $ group input

grow :: Int -> [Int] -> [Int]
grow n l = iterate process l !! n