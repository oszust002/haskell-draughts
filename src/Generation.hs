module Generation where
import Data.Tree
import Board
import Utils
import Moves

type GameInfo = (Color,Move, Board)

createGameInfo :: Board ->Color ->  Move -> GameInfo
createGameInfo board col move = (col, move, makeMove board move)

createTree :: GameInfo -> Tree GameInfo
createTree (col, move, board) = Node (col,move, board) (generateForest col board)

generateForest :: Color -> Board -> Forest GameInfo
generateForest col board =  map (createTree ) (map (createGameInfo board reverseCol) moves)
                            where reverseCol = getReverseColor col
                                  moves = getPermittedMoves col board

getNFromForest :: Int -> Forest a -> Forest a
getNFromForest n x = map (getNFromTree n) x

getNFromTree :: Int -> Tree a -> Tree a
getNFromTree 1 (Node a _) = Node a []
getNFromTree n (Node a b) = Node a $ getNFromForest (n-1) b

showGI :: GameInfo -> String
showGI (color, move, board) = show move

