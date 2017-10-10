data Comp a b = Comp a b

showComp :: (Show a, Show b) => Comp a b -> String
showComp (Comp x y) = show x ++ " + " ++ show y ++ "i"

instance (Show a, Show b) => Show (Comp a b) where
  show (Comp a b) = showComp (Comp a b)

instance Num a => Num (Comp a a) where
 (+) (Comp x1 y1) (Comp x2 y2) = Comp (x1 + x2) (y1 + y2)
 (*) (Comp x1 y1) (Comp x2 y2) = Comp x1 x2
 (-) (Comp x1 y1) (Comp x2 y2) = Comp 1 1
 abs (Comp a1 a2) = 3
 signum (Comp a1 a2) = 1
 fromInteger n = Comp 1 2