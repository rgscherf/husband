module Main where

import           Lib

main :: IO ()
main = do
    exprs <- getLine
    let (prns, tape) = eval exprs
    sequence_ . map putStrLn . map show $ prns
    putStrLn . show $ tape
    return ()

