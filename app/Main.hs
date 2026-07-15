module Main (main) where

import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    -- TODO: Uncomment the code below to pass the first stage
    putStr "$ "
    hFlush stdout
    cmd <- getLine
    let args = words cmd 
    case head args of 
        "exit" -> exitSuccess
        "echo" -> putStrLn $ unwords $ drop 1 args 
        _ -> putStrLn $ cmd ++ ": command not found"
    hFlush stdout
    main
    pure ()
