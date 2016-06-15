module Generation where
import Data.List
import Data.Tree
import Board
import Utils
import Moves
import Evaluation

type GameInfo = (Color,Move, Board)

createGameInfo :: Board ->Color ->  Move -> GameInfo
createGameInfo board col move = (col, move, makeMove board move)

createTree :: GameInfo -> Tree GameInfo
createTree (col, move, board) = Node (col,move, board) (generateForest reverseCol board)
                            where reverseCol = getReverseColor col

generateForest :: Color -> Board -> Forest GameInfo
generateForest col board =  map (createTree ) (map (createGameInfo board col) moves)
                            where moves = getPermittedMoves col board

getNFromForest :: Int -> Forest a -> Forest a
getNFromForest n x = map (getNFromTree n) x

getNFromTree :: Int -> Tree a -> Tree a
getNFromTree 1 (Node a _) = Node a []
getNFromTree n (Node a b) = Node a $ getNFromForest (n-1) b

showGI :: GameInfo -> String
showGI (color, move, board) = show move

ordByFirst :: Ord a => (a,b) -> (a,b) -> Ordering
ordByFirst (a,_) (b,_)
             | a>b = GT
             | a<b = LT
             | otherwise = EQ


maximizeTree :: Color -> Tree GameInfo -> (Int, GameInfo)
maximizeTree color (Node (col,move,board) []) = (evalBoard color board, (col,move,board))
maximizeTree color (Node a x) = (value, a)
                        where value = fst $ minimumBy (ordByFirst) $ map (minimizeTree color) x

minimizeTree :: Color -> Tree GameInfo -> (Int, GameInfo)
minimizeTree color (Node (col,move,board) []) = (evalBoard color board, (col,move,board))
minimizeTree color (Node a x) = (value, a)
                        where value = fst $ maximumBy (ordByFirst) $ map (maximizeTree color) x

minmaxBoard :: Color -> Forest GameInfo -> (Int, GameInfo)
minmaxBoard color forest = maximumBy (ordByFirst) $ map (minimizeTree color) forest