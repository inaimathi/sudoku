module Sudoku where

import Data.Set (Set(..), toList, fromList, union, difference)

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

solve :: Board -> Board
solve board = if newBoard == board then board else solve newBoard
  where newBoard = [map (\x -> newVal (x, y)) [0..8] | y <- [0..8]]
        newVal (x, y) = case (board !! y !! x, toList $ possibilities board (x, y)) of
          (0, [val]) -> val
          (val, _) -> val
          
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

mapply :: (a, b) -> [(a -> b -> c)] -> [c]
mapply args fns = map (\fn -> uncurry fn $ args) fns

intFloor :: Int -> Int -> Int
intFloor a b = fromEnum . floor . toEnum $ a `div` b