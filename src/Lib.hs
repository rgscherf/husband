{-# LANGUAGE RecordWildCards #-}

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
    | LockInc
    | LockDec
    deriving (Show)

inc, dec, right, left, loop, prn, lockInc, lockDec :: Parser Expr
inc = char '+' >> return Inc
dec = char '-' >> return Dec
right = char '>' >> return MvRight
left = char '<' >> return MvLeft
prn = char '.' >> return Print
lockInc = char '^' >> return LockInc
lockDec = char 'v' >> return LockDec
loop = do
    char '['
    exs <- many1 exprP
    char ']'
    return $ Loop exs

exprP :: Parser Expr
exprP = loop <|> inc <|> dec <|> right <|> left <|> prn <|> lockInc <|> lockDec

exprs :: Parser [Expr]
exprs = many exprP

---------------------------
-- MANAGING STATE: THE TAPE
---------------------------

initArr :: Array Int Int
initArr = array (-arrayBound, arrayBound)
                [ (n, 0) | n <- [(-arrayBound) .. arrayBound] ]
arrayBound = 10

data Tape = T { tTape :: Array Int Int
              , tPointer :: Int
              , tPrintQueue :: [Char]
              , tConstraints :: ConstraintMap
              } deriving (Show)

-- Incrementing and decrementing the pointer
modPtr :: (Int -> Int) -> Int -> Int -> Int
modPtr op other ptr =
    let n = op ptr
    in  if (n < -arrayBound) || (n > arrayBound) then other else n
incPtr = modPtr succ (-arrayBound)
decPtr = modPtr pred arrayBound

mkModErr :: String -> Int -> String -> String
mkModErr triedTo ptrPos markedAs =
    "Error. Tried to "
        ++ triedTo
        ++ " pointer at position "
        ++ show ptrPos
        ++ " but location was marked as "
        ++ markedAs
        ++ " only."

-- Modifying tape values
modCell
    :: CellOp
    -> (Array Int Int)
    -> Int
    -> ConstraintMap
    -> Either String (Array Int Int)
modCell op arr ptr cons
    = let
          opFn             = if op == IncOp then (+) else (-)
          arrWithOpApplied = arr // [(ptr, (opFn) (arr ! ptr) 1)]
      in
          case Data.Map.lookup ptr cons of
              Just NoCons  -> Right $ arrWithOpApplied
              Just OnlyDec -> if op == DecOp
                  then Right arrWithOpApplied
                  else Left $ mkModErr "increment" ptr "decrement"
              Just OnlyInc -> if op == IncOp
                  then Right arrWithOpApplied
                  else Left $ mkModErr "decrement" ptr "increment"
              _ ->
                  Left
                      $  "An erorr occurred. Index "
                      ++ show ptr
                      ++ " not found on the tape."

tryModCell :: CellOp -> Tape -> Either String Tape
tryModCell op tape@T {..} = case modCell op tTape tPointer tConstraints of
    Right arr' -> Right $ tape { tTape = arr' }
    Left  s    -> Left s

writeCons :: ConstraintMap -> Int -> CellCons -> ConstraintMap
writeCons cons key val =
    if val == NoCons then cons else update (\v -> Just val) key cons

tryWriteCons :: ConstraintMap -> Int -> CellCons -> Either String ConstraintMap
tryWriteCons cons key val = case Data.Map.lookup key cons of
    Just NoCons -> Right $ writeCons cons key val
    Nothing ->
        Left
            $  "Tried to write new constraint for pointer value "
            ++ show key
            ++ ", but was not found."
    Just c ->
        Left
            $  "Tried to write new constraint for pointer value "
            ++ show key
            ++ ", but constraint "
            ++ show c
            ++ " was already set."

wrapTryWriteCons lockVal tape@T {..} =
    case tryWriteCons tConstraints tPointer lockVal of
        Left  s -> Left s
        Right c -> Right $ tape { tConstraints = c }

-- try to evaluate exprs.
-- evalution can fail iff user tries to modify a tape cell with a previously-imposed constraint.
evalExprs :: [Expr] -> Either String Tape -> Either String Tape
evalExprs []       tape                = tape
evalExprs _        (Left  s          ) = Left s
evalExprs (e : es) (Right tape@T {..}) = case e of
    Print -> evalExprs es . Right $ tape
        { tPrintQueue = ((chr $ mod (tTape ! tPointer) 128) : tPrintQueue)
        }
    Inc        -> evalExprs es $ tryModCell IncOp tape
    Dec        -> evalExprs es $ tryModCell DecOp tape
    MvRight    -> evalExprs es $ Right tape { tPointer = incPtr tPointer }
    MvLeft     -> evalExprs es $ Right tape { tPointer = decPtr tPointer }
    LockInc    -> evalExprs es $ wrapTryWriteCons OnlyInc tape
    LockDec    -> evalExprs es $ wrapTryWriteCons OnlyDec tape
    Loop exprs -> case tTape ! tPointer of
        0 -> evalExprs es $ Right tape
        _ -> evalExprs (e : es) . evalExprs exprs $ Right tape

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

strToExprs s = case regularParse exprs s of
    Right e -> e
    Left  _ -> []

type HusbandResult = ([Char], [Int])
eval :: String -> HusbandResult
eval s =
    let exprs = strToExprs s
        tape  = Right $ T initArr 0 [] initConstraintMap
    in  case evalExprs exprs tape of
            Right T {..} -> (reverse tPrintQueue, elems tTape)
            Left  s      -> (s, [])

data CellOp
    = IncOp
    | DecOp
    deriving (Eq)

data CellCons
    = OnlyInc
    | OnlyDec
    | NoCons
    deriving (Eq, Show)

type ConstraintMap = Map Int CellCons

initConstraintMap :: Map Int CellCons
initConstraintMap = fromList [ (i, NoCons) | i <- [-arrayBound .. arrayBound] ]


