main :: IO ()
main = do
  interact (show . findBasement)

findBasement :: String -> Int
findBasement =
  length
    . takeWhile
      (/= -1)
    . scanl
      ( \acc x -> case x of
          '(' -> acc + 1
          ')' -> acc - 1
          _ -> error "unrecognizable input"
      )
      0
