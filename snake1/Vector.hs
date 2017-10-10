module Vector where

type Vector = (Int, Int)

addVectors :: Vector -> Vector -> Vector
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)