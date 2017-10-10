data Comp = Comp Float Float

instance Show Comp where
  show (Comp x y)
    | y == 0 = show x
    | y < 0 =  show x ++ " - " ++ show (y * (-1)) ++ "i"
    | otherwise = show x ++ " + " ++ show y ++ "i"

instance Num Comp where
  (+) (Comp x1 y1) (Comp x2 y2) = Comp (x1 + y1) (x2 + y2)
  (*) (Comp x1 y1) (Comp x2 y2) = Comp (x1 * x2 - y1 * y2) (x1 * y2 + x2 * y1)
  (-) (Comp x1 y1) (Comp x2 y2) = Comp (x1 - y1) (x2 - y2)
  abs (Comp x y) = Comp (abs x) (abs y)
  signum (Comp x y) = 1
  fromInteger n = Comp (fromInteger n) (fromInteger n)

class Arithmetic c where
  mag :: c -> Float
  conjugate :: c -> c
  multiplyScalar :: Float -> c -> c

instance Arithmetic Comp where
  mag (Comp x y) = sqrt (x * x + y * y)
  conjugate (Comp x y) = Comp x (-1 * y)
  multiplyScalar a (Comp x y) = Comp (x * a) (y * a)