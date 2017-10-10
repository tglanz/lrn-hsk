findValue :: Int -> [a] -> a
findValue i vec = vec !! i

just :: Maybe Int
just = Just 3

nothing :: Maybe Int
nothing = Nothing

get = do
    x <- Just 3
    return (x + 1)  

main = do
  putStrLn "start"

  let q = get

  return q