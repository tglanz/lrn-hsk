module Snake where

import Vector

type Snake = [Vector]

exampleSnake :: Snake
exampleSnake = [
    (3, 2),
    (3, 1),
    (2, 1),
    (1, 1),
    (0, 1),
    (0, 0)
  ]

newHeadPosition :: Snake -> Vector -> Vector
newHeadPosition snake direction = addVectors (head snake) direction

removeTail :: Snake -> Snake
removeTail = init

-- updateSnake :: snake -> isEating -> direction -> updatedSnake
updateSnake :: Snake -> Bool -> Vector -> Snake
updateSnake snake True direction = (newHeadPosition snake direction) : snake
updateSnake snake False direction = (newHeadPosition snake direction) : (removeTail snake)