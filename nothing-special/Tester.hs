type Predicate a = a -> Bool
data Tester a = Tested (Predicate a) a | Invalid

instance (Show a) => Show (Tester a) where
  show (Tested p a) = show a
  show Invalid = "invalid"

instance Functor Tester where
  fmap func (Tested p a)
    | p a == True = Tested (\_ -> True) (func a)
    | otherwise = Invalid
  fmap _ Invalid = Invalid 
