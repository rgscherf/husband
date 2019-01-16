module Lib where

import           Text.Parsec.String
import           Text.Parsec.Combinator
import           Text.Parsec.Char
import           Text.Parsec
import           Data.Array
import           Data.Char
import           Data.Map                          hiding ( (!)
                                                          , elems
                                                          )

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

data Tape = Tape (Array Int Int) Int [Char] ConstraintMap
     deriving (Show)

-- Incrementing and decrementing the pointer
modPtr :: (Int -> Int) -> Int -> Int -> Int
modPtr op other ptr =
    let n = op ptr
    in  if (n < -arrayBound) || (n > arrayBound) then other else n
incPtr = modPtr succ (-arrayBound)
decPtr = modPtr pred arrayBound

-- Modifying tape values
modTape
    :: (Int -> Int -> Int)
    -> (Array Int Int)
    -> Int
    -> ConstraintMap
    -> (Array Int Int)
modTape op arr ptr cons = case Data.Map.lookup ptr cons of
    Just NoCons -> arr // [(ptr, (op) (arr ! ptr) 1)]
    _           -> arr

incTape = modTape (+)
decTape = modTape (-)

evalExprs :: [Expr] -> Tape -> Tape
evalExprs []       tape                   = tape
evalExprs (e : es) (Tape arr ptr pq cons) = case e of
    Print ->
        evalExprs es (Tape arr ptr ((chr $ mod (arr ! ptr) 128) : pq) cons)
    Inc        -> evalExprs es (Tape (incTape arr ptr cons) ptr pq cons)
    Dec        -> evalExprs es (Tape (decTape arr ptr cons) ptr pq cons)
    MvRight    -> evalExprs es (Tape arr (incPtr ptr) pq cons)
    MvLeft     -> evalExprs es (Tape arr (decPtr ptr) pq cons)
    Loop exprs -> case arr ! ptr of
        0 -> evalExprs es $ Tape arr ptr pq cons
        _ -> evalExprs (e : es) $ evalExprs exprs (Tape arr ptr pq cons)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

strToExprs s = case regularParse exprs s of
    Right e -> e
    Left  _ -> []

type HusbandResult = ([Char], [Int])
eval :: String -> HusbandResult
eval s =
    let exprs             = strToExprs s
        tape              = Tape initArr 0 [] initConstraintMap
        (Tape arr _ pq _) = evalExprs exprs tape
    in  (reverse pq, elems arr)

data CellCons
    = OnlyInc
    | OnlyDec
    | NoCons
    deriving (Show)

type ConstraintMap = Map Int CellCons

initConstraintMap :: Map Int CellCons
initConstraintMap = fromList [ (i, NoCons) | i <- [-arrayBound .. arrayBound] ]


