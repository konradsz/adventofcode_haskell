main :: IO ()
main = do
  interact (show . calculateFloor)

calculateFloor :: String -> Int
calculateFloor =
  foldl
    ( \acc x -> case x of
        '(' -> acc + 1
        ')' -> acc - 1
        _ -> error "unrecognizable input"
    )
    0