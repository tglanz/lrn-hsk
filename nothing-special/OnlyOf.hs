testF x = x `mod` 2 == 0

data Mb a = J a | N deriving Show

instance Functor Mb where
  fmap f (J a) = J (f a)
  fmap f N = N

instance Applicative Mb where
  pure = J
  (J f) <*> m = fmap f m
  N <*> N = N
  
instance Monad Mb where
  return a = J a
  J a >>= f = f a
  N >>= f = N

doit m1 m2 = do
  x <- m1
  y <- m2
  let z = x ^ y
  return z