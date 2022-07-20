import Data.List (group, isInfixOf, tails)
import System.Environment (getArgs)

main :: IO ()
main = do
  (input : _) <- getArgs
  input <- readFile input
  let l = lines input
  print $ part1 l
  print $ part2 l

part1 :: [String] -> Int
part1 = count isNice
  where
    isNice input =
      vowels input
        && doubleLetter input
        && noForbiddenStrings input

part2 :: [String] -> Int
part2 = count isNice
  where
    isNice input = (any matchingPairs . tails) input && xyx input

vowels :: String -> Bool
vowels input = count (`elem` ['a', 'e', 'i', 'o', 'u']) input >= 3

doubleLetter :: String -> Bool
doubleLetter input = any ((>= 2) . length) $ group input

noForbiddenStrings :: String -> Bool
noForbiddenStrings input =
  not $
    any
      (`elem` ["ab", "cd", "pq", "xy"])
      (zipWith (\a b -> [a, b]) input (tail input))

matchingPairs :: String -> Bool
matchingPairs (a : b : xs) = [a, b] `isInfixOf` xs
matchingPairs _ = False

xyx :: String -> Bool
xyx (a : b : c : xs)
  | a == c = True
  | otherwise = xyx (b : c : xs)
xyx _ = False

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f