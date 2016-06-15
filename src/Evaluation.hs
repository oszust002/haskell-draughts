module Evaluation where
import Data.Maybe
import Board
import Utils

evaluatePosition :: Pos -> Int
evaluatePosition (x,y)
                | x == 0 || x == 7 || y == 0 || y == 7 = 4
                | x == 1 || x == 6 || y == 1 || y == 6 = 3
                | x == 2 || x == 5 || y == 2 || y == 5 = 2
                | x == 3 || x == 4 || y == 3 || y == 4 = 1
                | otherwise = 0

evaluateFigure :: Field -> Int
evaluateFigure Nothing = 0
evaluateFigure (Just (ColoredFigure _ Pawn)) = 1
evaluateFigure (Just (ColoredFigure _ King)) = 2

evalColor :: Color -> Color -> Int
evalColor a b
        | a == b = 1
        | otherwise = -1

evalField :: Color -> Board -> Pos -> Int
evalField col board pos
        | isNothing field = 0
        | otherwise = (evalColor col color) * (evaluateFigure field) * (evaluatePosition pos)
        where field = getField board pos
              color = getColor $ fromJust field

evalBoard :: Color -> Board -> Int
evalBoard col board =foldl (+) 0 [evalField col board (a,b) | a<-[0..7], b<-[0..7]]

