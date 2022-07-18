import qualified Data.Set as Set
import System.Environment (getArgs)

main :: IO ()
main = do
  (input : _) <- getArgs
  input <- readFile input
  print $ part1 input

part1 :: String -> Int
part1 =
  length . fst
    . foldl
      ( \(visited, (x, y)) dir ->
          let newPos = move dir (x, y)
           in (Set.insert newPos visited, newPos)
      )
      (Set.singleton (0, 0), (0, 0))

move :: Char -> (Int, Int) -> (Int, Int)
move dir (x, y) = case dir of
  '^' -> (x, y + 1)
  'v' -> (x, y - 1)
  '>' -> (x + 1, y)
  '<' -> (x - 1, y)
  _ -> error "incorrect input"
