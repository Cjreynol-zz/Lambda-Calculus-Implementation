import Reduction
import Parser
import Term
import ToLaTeX

main :: IO ()
main = mainLoop 0

mainLoop :: Int -> IO ()
mainLoop n = do
                putStrLn "Please enter a lambda term:  "
                input <- getLine
                putStrLn "Reduction sequence:"
                processInput input ("out/reduction" ++ (show n) ++ ".tex")
                mainLoop (n+1)


processInput :: String -> String -> IO ()
processInput input filename = case (myParser input) of
            (Left err) -> putStrLn (show err)
            (Right term) -> do
                            putStrLn $ (termListToStr . getReductionSeq) term
                            writeFile filename (preamble ++ ((termListToLaTeX . getReductionSeq) term) ++ conclusion)
