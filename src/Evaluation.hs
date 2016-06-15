module Evaluation where
import Data.Maybe
import Board
import Utils

positionValues = [[7*4,7*4,7*4,7*4,7*4,7*4,7*4,7*4],
                  [8*4,8*3,8*3,8*3,8*3,8*3,8*3,8*4],
                  [9*4,9*3,9*2,9*2,9*2,9*2,9*3,9*4],
                  [10*4,10*3,10*2,10*1,10*1,10*2,10*3,10*4],
                  [11*4,11*3,11*2,11*1,11*1,11*2,11*3,11*4],
                  [12*4,12*3,12*2,12*2,12*2,12*2,12*3,12*4],
                  [13*4,13*3,13*3,13*3,13*3,13*3,13*3,13*4],
                  [14*4,14*4,14*4,14*4,14*4,14*4,14*4,14*4]]

evaluatePosition :: Color -> Pos -> Int
evaluatePosition Black (x,y) = positionValues!!x!!y
evaluatePosition White (x,y) = (reverse positionValues)!!x!!y

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
        | otherwise = (evalColor col color) * (evaluateFigure field) * (evaluatePosition col pos)
        where field = getField board pos
              color = getColor $ fromJust field

evalBoard :: Color -> Board -> Int
evalBoard col board =foldl (+) 0 [evalField col board (a,b) | a<-[0..7], b<-[0..7]]