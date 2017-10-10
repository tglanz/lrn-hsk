module Utils where

partition :: Int -> [a] -> [[a]]
partition count [] = []
partition count array = (take count array) : (partition count $ drop count array)

-- applicative concat
concatA :: Maybe String -> Maybe String -> Maybe String
concatA a b = pure (++) <*> a <*> b