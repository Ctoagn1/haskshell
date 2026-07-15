module Main (main) where

import System.IO (hFlush, stdout)

main :: IO ()
main = do
    -- TODO: Uncomment the code below to pass the first stage
    putStr "$ "
    hFlush stdout
    cmd <- getLine
    putStrLn $ cmd ++ ": command not found"
    hFlush stdout
    pure ()
