module Main where

import           Husband
import           System.Environment

main :: IO ()
main = do
    exprs <- getArgs
    let (prns, tape) = eval $ head exprs
    sequence_ . map putStrLn . map show $ prns
    putStrLn . show $ tape
    return ()

