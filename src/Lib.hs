module Lib where

import           Text.Parsec.String
import           Text.Parsec.Combinator
import           Text.Parsec.Char
import           Text.Parsec
import           Data.Array
import           Data.Char

data Expr
    = MvRight
    | MvLeft
    | Inc
    | Dec
    | Loop [Expr]
    | Print
    deriving (Show)

inc, dec, right, left, loop, prn :: Parser Expr
inc = char '+' >> return Inc
dec = char '-' >> return Dec
right = char '>' >> return MvRight
left = char '<' >> return MvLeft
prn = char '.' >> return Print
loop = do
    char '['
    exs <- many1 exprP
    char ']'
    return $ Loop exs

exprP :: Parser Expr
exprP = loop <|> inc <|> dec <|> right <|> left <|> prn

exprs :: Parser [Expr]
exprs = many exprP

---------------------------
-- MANAGING STATE: THE TAPE
---------------------------

initArr :: Array Int Int
initArr = array (-arrayBound, arrayBound)
                [ (n, 0) | n <- [(-arrayBound) .. arrayBound] ]
arrayBound = 10

data Tape = Tape (Array Int Int) Int [Char]
     deriving (Show)

-- Incrementing and decrementing the pointer
modPtr :: (Int -> Int) -> Int -> Int -> Int
modPtr op other ptr =
    let n = op ptr
    in  if (n < -arrayBound) || (n > arrayBound) then other else n
incPtr = modPtr succ (-arrayBound)
decPtr = modPtr pred arrayBound

-- Modifying tape values
modTape :: (Int -> Int -> Int) -> (Array Int Int) -> Int -> (Array Int Int)
modTape op arr ptr = arr // [(ptr, (op) (arr ! ptr) 1)]
incTape = modTape (+)
decTape = modTape (-)

evalExprs :: [Expr] -> Tape -> Tape
evalExprs []       tape              = tape
evalExprs (e : es) (Tape arr ptr pq) = case e of
    Print      -> evalExprs es (Tape arr ptr ((chr $ mod (arr ! ptr) 128) : pq))
    Inc        -> evalExprs es (Tape (incTape arr ptr) ptr pq)
    Dec        -> evalExprs es (Tape (decTape arr ptr) ptr pq)
    MvRight    -> evalExprs es (Tape arr (incPtr ptr) pq)
    MvLeft     -> evalExprs es (Tape arr (decPtr ptr) pq)
    Loop exprs -> case arr ! ptr of
        0 -> evalExprs es $ Tape arr ptr pq
        _ -> evalExprs (e : es) $ evalExprs exprs (Tape arr ptr pq)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

strToExprs s = case regularParse exprs s of
    Right e -> e
    Left  _ -> []

type HusbandResult = ([Char], [Int])
eval :: String -> HusbandResult
eval s =
    let exprs             = strToExprs s
        tape              = Tape initArr 0 []
        (Tape arr ptr pq) = evalExprs exprs tape
    in  (reverse pq, elems arr)

