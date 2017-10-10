module Grid where

import Tile
import Tiles
import Utils

exampleTiles :: Tiles
exampleTiles = [
    Wall, Wall, Wall, Wall, Wall, Wall, Wall,
    Wall, Empty, Food, Empty, Empty, Empty, Wall,
    Wall, Food, Empty, Empty, Empty, Empty, Wall,
    Wall, Empty, Empty, Empty, Empty, Food, Wall,
    Wall, Wall, Wall, Wall, Wall, Wall, Wall
  ]

exampleGrid = Grid { cols = 7, tiles = exampleTiles }

data Grid = Grid {
    cols  :: Int,
    tiles :: Tiles
  } deriving (Show)

showGrid :: Grid -> Maybe String
showGrid (Grid { cols = cols, tiles = tiles }) = 
  foldl concatA (Just "") $ prefixWith "\n" tilesStrings
    where
      prefixWith p ms = (\m -> m >>= (\s -> Just(p ++ s))) <$> ms
      tilesStrings = showTiles <$> partition cols tiles