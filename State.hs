import Control.Monad.State

stateUpdater :: Int -> (Int, Char)
stateUpdater i = (i, 'a')