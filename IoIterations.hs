import Control.Monad.Loops

data World = World Int
  deriving (Show)

initialWorld :: World
initialWorld = World 1

updateWorld :: World -> World
updateWorld (World n) = World $ n + 1

shouldContinue :: World -> Bool
shouldContinue (World n) = n > 10

updateLoop :: World -> IO(World)
updateLoop world = do
  putStrLn $ show world
  return $ updateWorld world

main :: IO(World)
main = iterateUntilM shouldContinue updateLoop initialWorld