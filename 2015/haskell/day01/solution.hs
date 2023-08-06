import System.Environment (getArgs)

main :: IO ()
main = do
  (input : _) <- getArgs
  input <- readFile input
  print $ part1 input
  print $ part2 input

part1 :: String -> Int
part1 = foldl takeStep 0

part2 :: String -> Int
part2 = length . takeWhile (/= -1) . scanl takeStep 0

takeStep :: Int -> Char -> Int
takeStep floor dir =
  case dir of
    '(' -> floor + 1
    ')' -> floor - 1
    _ -> error "unrecognizable input"