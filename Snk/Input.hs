module Input where

data Input = IUp | IDown | ILeft | IRight
    deriving (Show, Eq, Enum)

inputChars :: [(Char, Input)]
inputChars = [
        ('w', IUp),
        ('a', ILeft),
        ('s', IDown),
        ('d', IRight)
    ]

toInput :: Char -> Maybe Input
toInput char = lookup char inputChars