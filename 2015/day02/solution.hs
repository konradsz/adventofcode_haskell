import Data.List
import System.Environment (getArgs)

main :: IO ()
main = do
  (input : _) <- getArgs
  input <- readFile input
  let presents = map (parseAsPresent . splitOnChar 'x') $ lines input
  print $ part1 presents
  print $ part2 presents

part1 :: [Present] -> Int
part1 = sum . map calculateWrappingPaperArea

part2 :: [Present] -> Int
part2 = sum . map calculateRibbonLength

type Present = (Int, Int, Int)

parseAsPresent :: [String] -> Present
parseAsPresent [w, h, l] = (read w, read h, read l)
parseAsPresent _ = error "incorrect input"

calculateWrappingPaperArea :: Present -> Int
calculateWrappingPaperArea (w, h, l) = 2 * s1 + 2 * s2 + 2 * s3 + minimum [s1, s2, s3]
  where
    s1 = l * w
    s2 = w * h
    s3 = h * l

calculateRibbonLength :: Present -> Int
calculateRibbonLength (w, h, l) = 2 * s1 + 2 * s2 + w * h * l
  where
    [s1, s2] = take 2 $ sort [w, h, l]

splitOnChar :: Char -> String -> [String]
splitOnChar c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitOnChar c s''
    where
      (w, s'') = break (== c) s'
