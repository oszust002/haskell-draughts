module Utils where
import Board
isInBounds :: Pos->Bool
isInBounds (a,b) = (a<8 && a>=0 && b<8 && b>=0)

isEmpty :: Board -> Pos -> Bool
isEmpty board pos = field == Nothing where field = getField board pos

getField :: Board -> Pos -> Field
getField a (b,c) = (a!!b)!!c

getFigure :: ColoredFigure -> Figure
getFigure (ColoredFigure _ figure) = figure

replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

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