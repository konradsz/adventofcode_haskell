import Data.List (group)
import System.Environment (getArgs)

main :: IO ()
main = do
  (input : _) <- getArgs
  input <- readFile input
  print $ part1 $ lines input

part1 :: [String] -> Int
part1 input = length $ filter isNice input

isNice :: String -> Bool
isNice input = hasAtLeast3Vowels input && hasDoubleLetter input && doesNotContainStrings input

hasAtLeast3Vowels :: String -> Bool
hasAtLeast3Vowels input = length (filter (`elem` ['a', 'e', 'i', 'o', 'u']) input) >= 3

hasDoubleLetter :: String -> Bool
hasDoubleLetter input = any ((>= 2) . length) $ group input

doesNotContainStrings :: String -> Bool
doesNotContainStrings input =
  not $
    any
      (`elem` ["ab", "cd", "pq", "xy"])
      (zipWith (\a b -> [a, b]) input (tail input))
