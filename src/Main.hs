{-|
Module      : Main
Description : Command-line interface for parsing and evaluating terms
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Main(
    main
    ) where


import LambdaTerm   (showReductionSteps)
import Pure.Parser  (termParser)


-- | Main loop of execution. 
-- 
-- Prompts for a lambda term.  The term will then be reduced to normal form 
-- and the full step-by-step reduction sequence will be displayed.
main :: IO ()
main = do
        putStrLn "\nPlease enter a lambda term:  "
        input <- getLine
        putStrLn "Reduction sequence:"
        processInput input
        main

processInput :: String -> IO ()
processInput input = 
    case (termParser input) of
        (Left err) -> putStrLn (show err)
        (Right (Left err)) -> putStrLn err
        (Right (Right t)) -> putStrLn $ showReductionSteps t

