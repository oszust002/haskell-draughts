module Board where
import Data.Maybe
import Data.List (maximumBy)
import Data.Function (on)

data Color = White | Black deriving (Show, Eq)
data Figure = King | Pawn deriving (Show, Eq)
data ColoredFigure = ColoredFigure Color Figure deriving Eq
type Field = Maybe ColoredFigure
type Board = [[Field]]
type Pos = (Int, Int)
data Move = SMove Pos Pos | Jump [Pos] deriving Show
data Direction = NW | NE | SW | SE deriving Eq

instance Show ColoredFigure where
    show (ColoredFigure White King) = "W "
    show (ColoredFigure Black King) = "B "
    show (ColoredFigure White Pawn) = "w "
    show (ColoredFigure Black Pawn) = "b "

printField :: Field -> String
printField Nothing = ". "
printField (Just a) = show a

rowString :: (Int,[Field]) -> String
rowString (a,b) =show a ++ " " ++ concatMap printField b


boardToString :: Board -> String
boardToString = (unlines . map (rowString ) . zip [0..7])

showBoard :: Board -> IO()
showBoard a = putStr ("  0 1 2 3 4 5 6 7\n" ++ boardToString a)

getField :: Board -> Pos -> Field
getField a (b,c) = (a!!b)!!c

getFigure :: ColoredFigure -> Figure
getFigure (ColoredFigure _ figure) = figure

replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

placeFigure :: Board -> Pos -> Field -> Board
placeFigure board (b,c) newField = replaceNth b (replaceNth c newField list) board where list = board!!b

deleteFigure :: Board -> Pos -> Board
deleteFigure board b = placeFigure board b Nothing

moveFigure :: Board->Pos->Pos->Board
moveFigure board b c = placeFigure (deleteFigure board b) c field where field = getField board b

makeJump :: Board->Move->Board
makeJump board (Jump [x]) = board
makeJump board (Jump (a:b:xs)) = makeJump (deleteFigure (moveFigure board a b) capturedPos) $ Jump $ b:xs
                where capturedPos = (x2+signum (x1-x2),y2+signum (y1-y2))
                      (x1,y1) = a
                      (x2,y2) = b

getColor :: ColoredFigure->Color
getColor (ColoredFigure color _) = color

isKing :: Field -> Bool
isKing (Just (ColoredFigure _ King)) = True
isKing _ = False

isPawn :: Field -> Bool
isPawn (Just (ColoredFigure _ Pawn)) = True
isPawn _ = False

getReverseColor :: Color -> Color
getReverseColor color
            | color == White = Black
            | color == Black = White

createLine :: Pos -> Int -> Direction ->  [Pos]
createLine (a,b) length NE = [(a+x,b+y) |  x<-[-length..(-1)], y<-[1..length], isInBounds (a+x,b+y), (a+x)+(b+y) == a-b]
createLine (a,b) length NW = [(a+x,b+y) |  x<-[-length..(-1)], y<-[-length..(-1)], isInBounds (a+x,b+y), a+x-(b+y) == a-b ]
createLine (a,b) length SE = [(a+x,b+y) |  x<-[1..length], y<-[1..length], isInBounds (a+x,b+y), (a+x)-(b+y) == a-b]
createLine (a,b) length SW = [(a+x,b+y) |  x<-[1..length], y<-[-length..(-1)], isInBounds (a+x,b+y), (a+x)+(b+y) == a+b]

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


getJump :: Color -> Pos -> Board -> [Pos]
getJump col pos board = if (color /= col) then [] else longest
                    where longest = maximumBy (compare `on` length) allJumps
                          allJumps = map (getJumpList) dirs
                          dirs = [NE,NW,SE,SW]
                          curField = getField board pos
                          color = getColor $ fromJust curField
                          getJumpList dir = if (field == Nothing) then [] else nextPos:(getJump col nextPos $ makeJump board $ Jump [pos,nextPos])
                                      where nextPos = getNextInDir enemyPos dir
                                            (enemyPos,field) = getEnemyOnDir board pos col dir maxLength
                                            maxLength = if (isKing curField) then 7 else 0
getAllJumps :: Color -> Board -> [Move]
getAllJumps col board = [Jump ((a,b):(getJump col (a,b) board))| a<-[0..7], b<-[0..7], getField board (a,b) /= Nothing]

notEmptyJump :: Move -> Bool
notEmptyJump (Jump []) = False
notEmptyJump (Jump [x]) = False
notEmptyJump (Jump _) = True

getPossibleJumps :: Color -> Board -> [Move]
getPossibleJumps col board = filter (notEmptyJump) $ getAllJumps col board

trackIsEmpty :: Board -> Move -> Bool
trackIsEmpty board  (SMove (a,b) (c,d))
                        | (c==a) = True
                        | otherwise = currentState && trackIsEmpty board nextStep
                        where currentState = isEmpty board (c,d)
                              nextStep = SMove (a,b) (c+signum(a-c),d+signum(b-d))

isInBounds :: Pos->Bool
isInBounds (a,b) = (a<8 && a>=0 && b<8 && b>=0)

isEmpty :: Board -> Pos -> Bool
isEmpty board pos = field == Nothing where field = getField board pos

initialBoard = [[Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn),Nothing,Nothing,Nothing,Just(ColoredFigure Black Pawn)],
                [Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn),Nothing],
                [Nothing,Just(ColoredFigure Black Pawn),Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                [Nothing,Nothing,Nothing,Nothing,Nothing,Just(ColoredFigure Black Pawn),Nothing,Nothing],
                [Just(ColoredFigure White King),Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing],
                [Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn)],
                [Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing]]