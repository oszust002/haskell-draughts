module Board where

data Color = White | Black deriving (Show, Eq)
data Figure = King | Pawn deriving (Show, Eq)
data ColoredFigure = ColoredFigure Color Figure deriving Eq
type Field = Maybe ColoredFigure
type Board = [[Field]]
type Pos = (Int, Int)

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
placeFigure a (b,c) newField = replaceNth b (replaceNth c newField list) a where
                list = a!!b

deleteFigure :: Board -> Pos -> Board
deleteFigure a b = placeFigure a b Nothing

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