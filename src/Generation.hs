module Generation where
import Data.Tree
import Board
import Utils
import Moves


type GameInfo = (Move, Board)

createGameInfo :: Board -> Move -> (Move, Board)
createGameInfo board move = (move, makeMove board move)

createTree :: Color -> GameInfo -> Tree GameInfo
createTree col (move,board) = Node (move, board) (generateForest col board)

generateForest :: Color -> Board -> Forest GameInfo
generateForest col board =  map (createTree reverseCol) (map (createGameInfo board) moves)
                            where reverseCol = getReverseColor col
                                  moves = getPermittedMoves col board


getNFromForest :: Int -> Forest a -> Forest a
getNFromForest n x = map (getNFromTree n) x

getNFromTree :: Int -> Tree a -> Tree a
getNFromTree 1 (Node a _) = Node a []
getNFromTree n (Node a b) = Node a $ getNFromForest (n-1) b

showGI :: GameInfo -> String
showGI (move, board) = show move ++ "\nboard:\n" ++ boardToString board

