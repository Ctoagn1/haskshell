module Main (main) where

import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    -- TODO: Uncomment the code below to pass the first stage
    putStr "$ "
    hFlush stdout
    cmd <- getLine
    case cmd of 
        "exit" -> exitSuccess
        _ -> putStrLn $ cmd ++ ": command not found"
    hFlush stdout
    main
    pure ()
