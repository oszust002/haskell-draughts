module Board where
import Data.Maybe

data Color = White | Black deriving (Show, Eq)
data Figure = King | Pawn deriving (Show, Eq)
data ColoredFigure = ColoredFigure Color Figure deriving Eq
type Field = Maybe ColoredFigure
type Board = [[Field]]
type Pos = (Int, Int)
data Move = SMove Pos Pos | Jump [(Pos,Pos)] deriving Show
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

makeJump :: Board->Pos->Pos->Board
makeJump board a b = deleteFigure (moveFigure board a b) capturedPos
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


createLine :: Pos -> Direction ->  [Pos]
createLine (a,b) NE = [(a+x,b+y) |  x<-[-7..(-1)], y<-[1..7], isInBounds (a+x,b+y), (a+x)+(b+y) == a-b]
createLine (a,b) NW = [(a+x,b+y) |  x<-[-7..(-1)], y<-[-7..(-1)], isInBounds (a+x,b+y), a+x-(b+y) == a-b ]
createLine (a,b) SE = [(a+x,b+y) |  x<-[1..7], y<-[1..7], isInBounds (a+x,b+y), (a+x)-(b+y) == a-b]
createLine (a,b) SW = [(a+x,b+y) |  x<-[1..7], y<-[-7..(-1)], isInBounds (a+x,b+y), (a+x)+(b+y) == a+b]

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
getKingMoves pos = (map (SMove pos) . concatMap (createLine pos)) dirs where dirs = [NE,NW,SE,SW]

getMoves :: Board -> Pos -> Color -> [Move]
getMoves board pos col
            | field == Just (ColoredFigure col Pawn) = getPawnMove col pos
            | field == Just (ColoredFigure col King) = getKingMoves pos
            | otherwise = []
            where field = getField board pos

getAllMoves :: Board -> Color->[Move]
getAllMoves board col = concat [getMoves board (a,b) col| a<-[0..7], b<-[0..7]]

getPossibleMoves :: Board -> Color -> [Move]
getPossibleMoves board col = filter (permittedMove board) $ getAllMoves board col

permittedMove :: Board -> Move -> Bool
permittedMove board move = isInBounds to && trackIsEmpty board move
                        where SMove from to = move



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

initialBoard = [[Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn)],
                [Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn),Nothing],
                [Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn)],
                [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                [Just(ColoredFigure White King),Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing],
                [Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn)],
                [Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing]
                ]