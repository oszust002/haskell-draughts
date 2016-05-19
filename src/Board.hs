module Board where
import Data.Maybe

data Color = White | Black deriving (Show, Eq)
data Figure = King | Pawn deriving (Show, Eq)
data ColoredFigure = ColoredFigure Color Figure deriving Eq
type Field = Maybe ColoredFigure
type Board = [[Field]]
type Pos = (Int, Int)
type Move = (Pos, Pos)

instance Show ColoredFigure where
    show (ColoredFigure White King) = "W "
    show (ColoredFigure Black King) = "B "
    show (ColoredFigure White Pawn) = "w "
    show (ColoredFigure Black Pawn) = "b "

printField :: Field -> String
printField Nothing = ". "
printField (Just a) = show a

rowString ::Int -> [Field] -> String
rowString a b =show a ++ " " ++ concatMap printField b


boardToString :: Board -> String
boardToString = unlines . map (rowString 1)

showBoard :: Board -> IO()
showBoard a = putStr (boardToString a)

getField :: Board -> Pos -> Field
getField a (b,c) = (a!!b)!!c

replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs


placeFigure :: Board -> Pos -> Field -> Board
placeFigure board (b,c) newField = replaceNth b (replaceNth c newField list) board where
                list = board!!b

deleteFigure :: Board -> Pos -> Board
deleteFigure board b = placeFigure board b Nothing

moveFigure :: Board->Move->Board
moveFigure board (b,c) = placeFigure (deleteFigure board b) c field where
        field = getField board b

getColor :: ColoredFigure->Color
getColor (ColoredFigure color _) = color


getMoveOfColor :: Color->Pos->[Move]
getMoveOfColor White (a,b) = [((a,b),(a-1,b-1)),((a,b),(a+1,b-1))]
getMoveOfColor Black (a,b) = [((a,b),(a-1,b+1)),((a,b),(a+1,b+1))]

possibleMoves :: Board->Color->Pos->[Move]
possibleMoves board color (a,b) = case field of
                                Nothing -> []
                                (Just fig) -> if (color == figColor) then getMoveOfColor color (a,b) else []
                                where
                                field = getField board (a,b)
                                figColor = getColor $ fromJust field

isInBounds :: Pos->Bool
isInBounds (a,b) = (a<8 && a>=0 && b<8 && b>=0)

getAllOfColor :: Board->Color->[Move]
getAllOfColor board color = concat [possibleMoves board color (a,b) | a <- [0..7], b<-[0..7]]


initialBoard = [[Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn)],
                [Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn),Nothing],
                [Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn),Nothing,Just(ColoredFigure Black Pawn)],
                [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                [Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing],
                [Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn)],
                [Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing,Just(ColoredFigure White Pawn),Nothing]
                ]