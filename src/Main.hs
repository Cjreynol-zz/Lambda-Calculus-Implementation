{-|
Module      : Main
Description : Command-line interface for parsing and evaluating terms
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Main(
    main
    ) where


import Reduction    (getReductionSeq)
import Parser       (termParser)
import Term         (termListToStr)


-- | Main loop of execution. 
-- 
-- Prompts for a lambda term written using standard De Bruijn notation and 
-- the '\' char for lambdas.  The term will then be reduced to normal form 
-- and the full step-by-step reduction sequence will be displayed.
main :: IO ()
main = do
        putStrLn "Please enter a lambda term:  "
        input <- getLine
        putStrLn "Reduction sequence:"
        processInput input
        main

processInput :: String -> IO ()
processInput input = case (termParser input) of
                        (Left err) -> putStrLn (show err)
                        (Right term) -> putStrLn $ (termListToStr . getReductionSeq) term

