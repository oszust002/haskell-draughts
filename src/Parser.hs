module Parser where
import System.Environment
import Control.Monad.Trans.State.Lazy
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Text.ParserCombinators.Parsec.Error
import System.IO
import Data.Maybe
import Data.Functor
import Control.Monad.IO.Class
import Board
import Moves

data PDN = Move (Int,Int) -- pozycja startowa i koncowa
           | Kill [Int]  -- pozycja startowa to glowa, pozniej kolejne pozycje
           deriving (Show,Eq)


fromIntToPos :: Int -> Pos
fromIntToPos a
        | (a<1 && a>32) = (-1,-1) --TODO: error or something
        | otherwise = (x,y) where x = (a-1) `div` 4
                                  y = if (even x) then 2*(a-4*x)-1 else 2*(a-4*x-1)

fromPosToInt :: Pos -> Int
fromPosToInt (x,y) = 4*x+(y `div` 2) + 1

movesToPDNString :: [Move] -> String
movesToPDNString [x] = pdnToString $ moveToPDN x
movesToPDNString (x:xs) = pdnToString  (moveToPDN x) ++ ", " ++ movesToPDNString xs

pdnToString :: PDN -> String
pdnToString (Move (a,b)) = show a ++ "-" ++ show b
pdnToString (Kill [x]) = show x
pdnToString (Kill (x:xs)) = show x ++ "x" ++ (pdnToString $ Kill xs)

pdnToMove :: PDN -> Move
pdnToMove (Move (a,b)) = SMove (fromIntToPos a) $ fromIntToPos b
pdnToMove (Kill x) = Jump $ map (fromIntToPos) x

moveToPDN :: Move -> PDN
moveToPDN (SMove a b) = Move (fromPosToInt a, fromPosToInt b)
moveToPDN (Jump x) = Kill $ map (fromPosToInt) x

parsePos :: Parser Int
parsePos = do
            x <- int
            if (x<1 || x>32) then
              unexpected "Tylko liczby od 1-32"
            else
              return x

parseMove = do
            x1 <- parsePos
            (char '-')
            x2 <- parsePos
            eof
            return $ Move (x1,x2)

parseKill = do
            x1 <- sepBy (parsePos) (char 'x')
            eof
            if (length x1) > 1 then
              return $ Kill x1
            else
              unexpected "start i koniec minimum"

parsePDN = try parseMove <|> parseKill

parseToMove :: String -> Maybe Move
parseToMove a = case parse parsePDN "PDNerror" a of
                    Right move -> Just (pdnToMove move)
                    Left x -> Nothing