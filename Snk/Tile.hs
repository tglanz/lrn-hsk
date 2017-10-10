module Tile
  (
    Tile (Empty, Food, Wall, Part), 
    showTile
  ) where

data Tile = Empty | Food | Wall | Part
  deriving (Show, Eq, Enum)

tileStrings = [
    (Empty, " "),
    (Wall, "#"),
    (Food, "@"),
    (Part, "O")
  ]

showTile :: Tile -> Maybe String
showTile tile = lookup tile tileStrings

toTile :: String -> Maybe Tile
toTile string = lookup string $ (\(a, b) -> (b, a)) <$> tileStrings
