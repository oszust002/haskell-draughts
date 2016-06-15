module Main where
import System.Environment
import Data.Maybe
import Moves
import Board
import Generation
import Parser
import Utils

play color board = if (isWinner reverseColor board)
                       then (putStrLn  (show reverseColor ++ " wins!"))
                       else do
                           showBoard board
                           putStrLn "Your move:"
                           move <- getLine
                           parsed <- return $ parseToMove move
                           if (isNothing parsed)
                               then putStrLn "Wrong value(correct is from 1 to 32) or invalid format of move, try this \nMoves: 1-2\nJumps: 1x2x3" >> play color board
                               else if ((fromJust parsed) `elem` permittedMoves)
                                   then playAutoPlay reverseColor $ makeMove board $ fromJust parsed
                                   else putStrLn ("Move not permitted, permitted moves: \n" ++  permMovesString) >> play color board
                    where permittedMoves = getPermittedMoves color board
                          reverseColor = getReverseColor color
                          permMovesString = movesToPDNString permittedMoves

playAutoPlay color board = if (isWinner reverseColor board)
                               then (putStrLn  (show reverseColor ++ " wins!"))
                               else do
                                    showBoard board
                                    putStrLn $ pdnToString $ moveToPDN bestMove
                                    play reverseColor (makeMove board bestMove)
                           where bestMove = getTheBestMove color board 7
                                 reverseColor = getReverseColor color

countFigs :: Color -> Board -> Int
countFigs color board =foldl (+) 0 $ map (length) $ map (filter (\x -> (not (isNothing x)) && ((getColor (fromJust x)) == color) )) board

isWinner :: Color -> Board -> Bool
isWinner color board
         | (countFigs color board /= 0) && (countFigs reverseColor board == 0) = True
         | (length permMoves == 0) = True
         | otherwise = False
         where reverseColor = getReverseColor color
               permMoves = getPermittedMoves reverseColor board

main = do
       args <- getArgs
       if (length args == 0)
            then putStrLn "Put argument!"
            else do
                 color <- return (args!!0)
                 case color of
                      "black" -> playAutoPlay White initBoard
                      "white" -> play White initBoard
                      otherwise -> putStrLn "Wrong argument! Try 'black' or 'white'"
