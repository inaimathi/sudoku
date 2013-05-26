module Sudoku where

import Data.Set (Set(..), toList, fromList, union, difference)
import Data.List (sortBy)
import Data.Ord (comparing)

type Board = [[Int]]

row :: Board -> (Int, Int) -> Set Int
row board (x, y) = fromList $ board !! y

col :: Board -> (Int, Int) -> Set Int
col board (x, y) = fromList $ map (!! x) board

block :: Board -> (Int, Int) -> Set Int
block board (x, y) = fromList . concat . map (take blockSize . drop ox) $ take blockSize $ drop oy board
  where blockSize = fromEnum . sqrt . toEnum $ length board
        origin n = blockSize * intFloor n blockSize
        ox = origin x
        oy = origin y

possibilities :: Board -> (Int, Int) -> Set Int
possibilities board (x, y) = foldl difference (fromList [1..9]) sets
  where sets = mapply (board, (x, y)) [row, col, block]

empties :: Board -> Bool
empties board = any id $ map (any (== 0)) board

solve :: Board -> Board
solve board = case (obvious == board, empties obvious) of
  (True, False) -> board
  (True, True) -> obvious -- call guess here
  (False, _) -> solve obvious
  where ixs = [0..(length board - 1)]
        obvious = [map (\x -> newVal (x, y)) ixs | y <- ixs]
        newVal (x, y) = case (board !! y !! x, toList $ possibilities board (x, y)) of
          (0, [val]) -> val
          (val, _) -> val

guess board = sortBy (comparing pLen) [(x, y, toList $ possibilities board (x, y)) | x <- ixs, y <- ixs, 0 == (board !! y !! x)]
  where ixs = [0..(length board - 1)]
        pLen (x, y, p) = length p

---------- Utility and sample data
sample :: Board
sample = [[0, 7, 1, 4, 0, 0, 0, 0, 5],
          [0, 0, 0, 0, 5, 0, 0, 8, 0],
          [0, 0, 3, 9, 0, 7, 6, 0, 0],
          [0, 0, 0, 0, 0, 1, 0, 0, 0],
          [0, 9, 0, 8, 0, 6, 0, 0, 3],
          [0, 0, 0, 0, 0, 0, 8, 2, 0],
          [0, 6, 0, 0, 4, 0, 7, 0, 8],
          [3, 0, 0, 0, 0, 0, 0, 9, 0],
          [0, 0, 0, 0, 8, 5, 0, 0, 0]]

-- [[0,7,1,4,0,8,0,3,5],[0,0,0,0,5,3,0,8,7],[0,0,3,9,0,7,6,0,0],[0,0,0,0,0,1,0,0,0],[0,9,0,8,0,6,0,0,3],[0,0,0,0,0,4,8,2,0],[0,6,0,3,4,9,7,0,8],[3,0,0,0,0,2,0,9,0],[0,0,0,0,8,5,3,0,0]]

mapply :: (a, b) -> [(a -> b -> c)] -> [c]
mapply args fns = map (\fn -> uncurry fn $ args) fns

intFloor :: Int -> Int -> Int
intFloor a b = fromEnum . floor . toEnum $ a `div` b

---------- Prining
printBoard :: Board -> IO ()
printBoard board = printChunk board
  where blockSize = fromEnum . sqrt . toEnum $ length board
        delim = replicate (blockSize + (2 * length board)) '-'
        ch = chunk blockSize
        printChunk = ch printLine (putStrLn delim) (putStr "")
        printLine = ch printSquare (putStr "|") (putStrLn "")
        printSquare 0 = putStr "  "
        printSquare num = putStr . (++ " ") $ show num

chunk :: Int -> (a -> IO ()) -> IO () -> IO () -> [a] -> IO ()
chunk chunkSize each delim finally corpus = rec corpus
  where rec [] = finally
        rec piece = do mapM_ each $ take chunkSize piece
                       delim
                       rec $ drop chunkSize piece