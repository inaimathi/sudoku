module Sudoku where

import Data.Set (Set(..), toList, fromList, union, difference, intersection)
import qualified Data.Set as Set
import Data.List (sortBy, intercalate)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import Data.Char (intToDigit)

data Board = Board { values :: [[Int]], 
                     empty :: Set (Int, Int),
                     size :: Int, 
                     ixs :: [Int],
                     blockSize :: Int } deriving (Eq)

instance Show Board where
  show board = unlines . intercalate hdelim . split . lns $ values board
    where lns = map (intercalate "|" . split . map sq)
          split = chunksOf bs
          sq n = if n == 0 then ' ' else intToDigit n
          hdelim = [replicate (size board + (bs - 1)) '-']
          bs = blockSize board

row :: Board -> (Int, Int) -> Set Int
row board (x, y) = fromList $ values board !! y

col :: Board -> (Int, Int) -> Set Int
col board (x, y) = fromList . map (!! x) $ values board

block :: Board -> (Int, Int) -> Set Int
block board (x, y) = fromList . concat . square $ values board
  where square = map (take bs . drop (origin x)) . take bs . drop (origin y)
        origin n = bs * intFloor n bs
        bs = blockSize board

blockEmpties :: Board -> (Int, Int) -> [(Int, Int)]
blockEmpties board (x, y) = [(x', y') | x' <- xs, y' <- ys, blank (x', y')]
  where blank (x, y) = 0 == ((values board) !! y !! x)
        xs = [ox..ox + bs]
        ys = [oy..oy + bs]
        [ox, oy] = map origin [x, y]
        origin n = bs * intFloor n bs
        bs = (blockSize board)-1

possibilities :: Board -> (Int, Int) -> Set Int
possibilities board (x, y) = foldl difference (fromList [1..size board]) sets
  where sets = mapply (board, (x, y)) [row, col, block]

solve :: Board -> Board
solve board = case (new == board, Set.size $ empty new) of
  (_, 0) -> new
  (True, _) -> new -- next stage
  (False, _) -> solve new
  where new = obvious board

obvious board = findEmpties $ board { values = newVals }
  where newVals = [map (\x -> newVal (x, y)) $ ixs board | y <- ixs board]
        ps x y = toList $ possibilities board (x, y)
        newVal (x, y) = case ((values board) !! y !! x, ps x y) of
          (0, [val]) -> val
          (val, _) -> val

-- neededInBlock board (x, y) = difference (fromList [1..size board]) $ block board (x, y)
-- blockPos board (x, y) = map (possibilities board) $ blockEmpties board (x, y)
-- uniqueInBlock board (x, y) = foldl intersection $ block board (x, y):ps
--   where ps = map (possibilities board) $ blockEmpties board (x, y)
-- blockWise board = findEmpties $ board { values = newVals }
--   where 

---------- Data-related
toBoard :: [[Int]] -> Board
toBoard values = findEmpties $ Board { values = values, empty = fromList [],
                                       size = len, ixs = [0..len - 1], blockSize = bs }
  where bs = fromEnum . sqrt . toEnum $ length values
        len = length values

findEmpties :: Board -> Board
findEmpties board = board { empty = fromList [(x, y) | y <- is, x <- is, blank (x, y)] }
  where blank (x, y) = 0 == ((values board) !! y !! x)
        is = ixs board

sample = toBoard [[0,7,1,4,0,0,0,0,5],
                  [0,0,0,0,5,0,0,8,0],
                  [0,0,3,9,0,7,6,0,0],
                  [0,0,0,0,0,1,0,0,0],
                  [0,9,0,8,0,6,0,0,3],
                  [0,0,0,0,0,0,8,2,0],
                  [0,6,0,0,4,0,7,0,8],
                  [3,0,0,0,0,0,0,9,0],
                  [0,0,0,0,8,5,0,0,0]]

---------- Utility
mapply :: (a, b) -> [(a -> b -> c)] -> [c]
mapply args fns = map (\fn -> uncurry fn $ args) fns

intFloor :: Int -> Int -> Int
intFloor a b = fromEnum . floor . toEnum $ a `div` b

thd :: (a, b, c) -> c
thd (a, b, c) = c