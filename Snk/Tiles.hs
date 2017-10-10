module Tiles where

import Control.Monad

import Tile
import Utils

type Tiles = [Tile]

showTiles :: Tiles -> Maybe String
showTiles tiles = foldl concatA (Just "") $ showTile <$> tiles