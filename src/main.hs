import Reduction
import Parser


main :: IO ()
main = mainLoop 0

mainLoop :: Int -> IO ()
mainLoop n = do
                input <- getLine
                processInput input
                mainLoop (n+1)


processInput :: String -> IO ()
processInput input = case (myParser input) of
                        (Left err) -> putStrLn (show err)
                        (Right term) -> (putStrLn . termToRedSeqStr) term
