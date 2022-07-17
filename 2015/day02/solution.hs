import Data.List
import System.Environment (getArgs)

main :: IO ()
main = do
  (input : _) <- getArgs
  input <- readFile input
  print $ part1 input
  print $ part2 input

data Present = Present Int Int Int deriving (Show)

calculateWrappingPaperArea :: Present -> Int
calculateWrappingPaperArea (Present w h l) = 2 * s1 + 2 * s2 + 2 * s3 + minimum [s1, s2, s3]
  where
    s1 = l * w
    s2 = w * h
    s3 = h * l

calculateRibbonLength :: Present -> Int
calculateRibbonLength (Present w h l) = 2 * s1 + 2 * s2 + w * h * l
  where
    sorted = sort [w, h, l]
    s1 = head sorted
    s2 = sorted !! 1

splitOnChar :: Char -> String -> [Int]
splitOnChar c s = case dropWhile (== c) s of
  "" -> []
  s' -> read w : splitOnChar c s''
    where
      (w, s'') = break (== c) s'

part1 :: String -> Int
part1 input =
  sum
    . map
      ( (calculateWrappingPaperArea . \[w, h, l] -> Present w h l)
          . splitOnChar 'x'
      )
    $ lines input

part2 :: String -> Int
part2 input =
  sum
    . map
      ( (calculateRibbonLength . \[w, h, l] -> Present w h l)
          . splitOnChar 'x'
      )
    $ lines input