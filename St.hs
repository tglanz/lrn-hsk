import Data.List
import Control.Monad.Loops

-- Definitions

data Input = LeftI | RightI | UpI | DownI deriving (Show, Enum, Eq)
data Cube = ValueCube Int | EmptyCube deriving (Show, Eq, Ord)

type CubeVector = [Cube]
type CubeIndex = Int
type CubePosition = (Int, Int)
type ColsCount = Int

initialState :: [Cube]
initialState = [
  ValueCube 1, ValueCube 2, ValueCube 3,
  ValueCube 4, ValueCube 5, ValueCube 8,
  ValueCube 7, EmptyCube, ValueCube 6]

-- Parsing

inputToChar = [(LeftI, 'a'), (RightI, 'd'), (UpI, 'w'), (DownI, 's')]

parseCharToInput :: Char -> Maybe Input
parseCharToInput char = lookup char (map (\(a, b) -> (b, a)) inputToChar)

idxToCol :: ColsCount -> Int -> Int
idxToCol cols idx = idx `mod` cols

idxToRow :: ColsCount -> Int -> Int
idxToRow cols idx = idx `quot` cols

-- Logic

isEmptyCube :: Cube -> Bool
isEmptyCube EmptyCube = True
isEmptyCube (ValueCube _) = False

swapIndices :: CubeVector -> CubeIndex -> CubeIndex -> CubeVector
swapIndices vec i j
  | i == j = vec
  | otherwise = firstPartition ++ [maxElem] ++ middlePartition ++ [minElem] ++ lastPartition
  where
    minIdx = min i j
    maxIdx = max i j
    minElem = vec !! minIdx
    maxElem = vec !! maxIdx
    firstPartition = take minIdx vec
    lastPartition = drop (maxIdx + 1) vec
    middlePartition = drop (minIdx + 1) (take maxIdx vec)

findEmptyCubeIndex :: CubeVector -> CubeIndex
findEmptyCubeIndex cubes = findEmptyCubeIndex' cubes 0 where
  findEmptyCubeIndex' [cube] pos
    | isEmptyCube cube == True = pos
    | otherwise = -1
  findEmptyCubeIndex' (cube:cubes) pos
    | isEmptyCube cube == True = pos
    | otherwise = findEmptyCubeIndex' cubes (pos + 1)

findValueCubeIndexForInput :: Input -> ColsCount -> CubeIndex -> Maybe CubeIndex
findValueCubeIndexForInput RightI cols emptyCubeIndex = if (idxToCol cols emptyCubeIndex) == 0 then Nothing else Just $ emptyCubeIndex - 1
findValueCubeIndexForInput LeftI cols emptyCubeIndex = if (idxToCol cols emptyCubeIndex) == (cols - 1) then Nothing else Just $ emptyCubeIndex + 1
findValueCubeIndexForInput UpI cols emptyCubeIndex = if (idxToRow cols emptyCubeIndex) == (cols - 1) then Nothing else Just $ emptyCubeIndex + cols 
findValueCubeIndexForInput DownI cols emptyCubeIndex = if (idxToRow cols emptyCubeIndex) == 0 then Nothing else Just $ emptyCubeIndex - cols

isFinish :: CubeVector -> Bool
isFinish cubes = cubes == sort cubes

-- Shows

showCube :: Cube -> String
showCube EmptyCube = " "
showCube (ValueCube a) = show a 

showCubes :: CubeVector -> String
showCubes [] = ""
showCubes cubes = concat $ map ((\x -> x ++ " ") . showCube) cubes

showCubesMatrix :: ColsCount -> CubeVector -> String
showCubesMatrix _ [] = ""
showCubesMatrix cols cubes = (showCubes (take cols cubes)) ++ "\n" ++ (showCubesMatrix cols (drop cols cubes))

-- Interaction and IO

captureInput :: IO(Maybe Input)
captureInput = do
  char <- getChar
  putStr "\n\n"
  let input = parseCharToInput char
  return input

handleInput :: Maybe Input -> CubeVector -> ColsCount -> CubeVector
handleInput Nothing vec _ = vec
handleInput (Just input) vec cols = handleInput' cubeIndex emptyCubeIndex vec
  where
    cubeIndex = findValueCubeIndexForInput input cols emptyCubeIndex
    emptyCubeIndex = findEmptyCubeIndex vec
    handleInput' Nothing _ vec = vec
    handleInput' (Just valueCubeIndex) emptyCubeIndex vec = swapIndices vec valueCubeIndex emptyCubeIndex

-- Control

updateLoop :: ColsCount -> CubeVector -> IO(CubeVector)
updateLoop cols cubes = do
  input <- captureInput
  let updatedCubes = handleInput input cubes cols
  putStrLn (showCubesMatrix cols updatedCubes)
  return updatedCubes

main :: IO()
main = do
  putStrLn (showCubesMatrix 3 initialState)
  iterateUntilM isFinish (updateLoop 3) initialState
  putStrLn "Done"