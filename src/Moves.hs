module Moves where
import Data.Maybe
import Data.List (maximumBy)
import Data.Function (on)
import Board
import Utils

data Move = SMove Pos Pos | Jump [Pos] | OneJump Pos Pos deriving Show
data Direction = NW | NE | SW | SE deriving Eq

createLine :: Pos -> Int -> Direction ->  [Pos]
createLine (a,b) length NE = [(a+x,b+y) |  x<-[-length..(-1)], y<-[1..length], isInBounds (a+x,b+y), (a+x)+(b+y) == a-b]
createLine (a,b) length NW = [(a+x,b+y) |  x<-[-length..(-1)], y<-[-length..(-1)], isInBounds (a+x,b+y), a+x-(b+y) == a-b ]
createLine (a,b) length SE = [(a+x,b+y) |  x<-[1..length], y<-[1..length], isInBounds (a+x,b+y), (a+x)-(b+y) == a-b]
createLine (a,b) length SW = [(a+x,b+y) |  x<-[1..length], y<-[-length..(-1)], isInBounds (a+x,b+y), (a+x)+(b+y) == a+b]

placeFigure :: Board -> Pos -> Field -> Board
placeFigure board (b,c) newField = replaceNth b (replaceNth c newField list) board where list = board!!b

deleteFigure :: Board -> Pos -> Board
deleteFigure board b = placeFigure board b Nothing

checkPromotion :: Pos -> Color -> Bool
checkPromotion (a,b) White = a == 0
checkPromotion (a,b) Black = a == 7


moveFigure :: Board->Pos->Pos -> Bool -> Board
moveFigure board b c prom = placeFigure (deleteFigure board b) c field
                            where field
                                    | prom == False = getField board b
                                    | prom == True = if (checkPromotion c color) == True then Just (ColoredFigure color King) else getField board b
                                  color = getColor $ fromJust $ getField board b

makeMove :: Board -> Move -> Board
makeMove board (SMove from to) = moveFigure board from to True
makeMove board (Jump [x]) = board
makeMove board (OneJump x y) = deleteFigure (moveFigure board x y True) capturedPos
                where capturedPos = (x2+signum (x1-x2),y2+signum (y1-y2))
                      (x1,y1) = x
                      (x2,y2) = y

makeMove board (Jump [x,y]) = deleteFigure (moveFigure board x y True) capturedPos
                where capturedPos = (x2+signum (x1-x2),y2+signum (y1-y2))
                      (x1,y1) = x
                      (x2,y2) = y

makeMove board (Jump (a:b:xs)) = makeMove (deleteFigure (moveFigure board a b False) capturedPos) $ Jump $ b:xs
                where capturedPos = (x2+signum (x1-x2),y2+signum (y1-y2))
                      (x1,y1) = a
                      (x2,y2) = b

getNeighbour :: Pos -> Direction -> Pos
getNeighbour (a,b) dir
            | dir == NE = (a-1,b+1)
            | dir == NW = (a-1,b-1)
            | dir == SE = (a+1,b+1)
            | dir == SW = (a+1,b-1)

getPawnMove :: Color -> Pos -> [Move]
getPawnMove col pos
            | col == White = [SMove pos $ getNeighbour pos NW, SMove pos $ getNeighbour pos NE]
            | col == Black = [SMove pos $ getNeighbour pos SW, SMove pos $ getNeighbour pos SE]

getKingMoves :: Pos -> [Move]
getKingMoves pos = (map (SMove pos) . concatMap (createLine pos 7)) dirs where dirs = [NE,NW,SE,SW]

getMoves :: Board -> Pos -> Color -> [Move]
getMoves board pos col
            | field == Just (ColoredFigure col Pawn) = getPawnMove col pos
            | field == Just (ColoredFigure col King) = getKingMoves pos
            | otherwise = []
            where field = getField board pos

getAllMoves :: Color -> Board->[Move]
getAllMoves col board = concat [getMoves board (a,b) col| a<-[0..7], b<-[0..7]]

getPossibleMoves :: Color -> Board -> [Move]
getPossibleMoves col board = filter (permittedMove board) $ getAllMoves col board

permittedMove :: Board -> Move -> Bool
permittedMove board move = isInBounds to && trackIsEmpty board move where SMove from to = move

getNextInDir :: Pos -> Direction -> Pos
getNextInDir (x,y) dir
            | dir == NE = (x-1,y+1)
            | dir == NW = (x-1,y-1)
            | dir == SE = (x+1,y+1)
            | dir == SW = (x+1,y-1)

getEnemyOnDir :: Board -> Pos -> Color -> Direction -> Int -> (Pos,Field)
getEnemyOnDir board pos col direction step
            | not (isInBounds next) || not (isInBounds nextNext) = (pos, Nothing)
            | isEmpty board next && step == 0 = (pos, Nothing)
            | isEmpty board next && (step>0) = getEnemyOnDir board next col direction (step-1)
            | col == nextCol = (pos, Nothing)
            | not (isEmpty board nextNext) = (pos, Nothing)
            | otherwise = (next, nextField)
            where next = getNextInDir pos direction
                  nextNext = getNextInDir next direction
                  nextField = getField board next
                  nextNextField = getField board nextNext
                  nextCol = getColor $ fromJust nextField


addToEveryList :: a -> [[a]] -> [[a]]
addToEveryList a [] = [[a]]
addToEveryList a b = map (a:) b


getAllMaxLists :: [[a]]->[[a]]
getAllMaxLists a = filter (\x -> length x == maxLength) a where maxLength = length $ maximumBy (compare `on` length) a

getJump :: Color -> Pos -> Board -> [[Pos]]
getJump col pos board = if (color /= col) then [] else longest
                    where longest = getAllMaxLists allJumps
                          allJumps = concatMap (getJumpList) dirs
                          dirs = [NE,NW,SE,SW]
                          curField = getField board pos
                          color = getColor $ fromJust curField
                          getJumpList dir = if (field == Nothing) then [] else addToEveryList nextPos $ getJump col nextPos $ makeMove board $ Jump [pos,nextPos]
                                      where nextPos = getNextInDir enemyPos dir
                                            (enemyPos,field) = getEnemyOnDir board pos col dir maxLength
                                            maxLength = if (isKing curField) then 7 else 0

getAllJumps :: Color -> Board -> [Move]
getAllJumps col board = ((map (Jump)) . concat) [(addToEveryList (a,b) (getJump col (a,b) board))| a<-[0..7], b<-[0..7], getField board (a,b) /= Nothing]

notEmptyJump :: Move -> Bool
notEmptyJump (Jump []) = False
notEmptyJump (Jump [x]) = False
notEmptyJump (Jump _) = True

getPossibleJumps :: Color -> Board -> [Move]
getPossibleJumps col board = filter (notEmptyJump) $ getAllJumps col board

getPermittedMoves :: Color -> Board -> [Move]
getPermittedMoves col board
        | length jumps == 0 = moves
        | otherwise = jumps
        where jumps = getPossibleJumps col board
              moves = getPossibleMoves col board


trackIsEmpty :: Board -> Move -> Bool
trackIsEmpty board  (SMove (a,b) (c,d))
                        | (c==a) = True
                        | otherwise = currentState && trackIsEmpty board nextStep
                        where currentState = isEmpty board (c,d)
                              nextStep = SMove (a,b) (c+signum(a-c),d+signum(b-d))