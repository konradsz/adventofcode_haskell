import Data.List (group)
import System.Environment (getArgs)

main :: IO ()
main = do
  (input : _) <- getArgs
  input <- readFile input
  let l = lines input
  print $ part1 l

part1 :: [String] -> Int
part1 = count isNice

isNice :: String -> Bool
isNice input = hasAtLeast3Vowels input && hasDoubleLetter input && doesNotContainStrings input

hasAtLeast3Vowels :: String -> Bool
hasAtLeast3Vowels input = count (`elem` ['a', 'e', 'i', 'o', 'u']) input >= 3

hasDoubleLetter :: String -> Bool
hasDoubleLetter input = any ((>= 2) . length) $ group input

doesNotContainStrings :: String -> Bool
doesNotContainStrings input =
  not $
    any
      (`elem` ["ab", "cd", "pq", "xy"])
      (zipWith (\a b -> [a, b]) input (tail input))

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f