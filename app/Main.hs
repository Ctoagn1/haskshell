module Main (main) where

import System.IO (hFlush, stdout)


data EvaluatedResult = PrintAndContinue String | Exit | Continue

main :: IO ()
main = do
    putStr "$ "
    hFlush stdout
    cmd <- getLine
    handleEval $ eval cmd
    
eval :: String -> EvaluatedResult
eval args = if null args then Continue else eval' (getCommand args) (getRemainingArgs args)

eval' :: String -> String -> EvaluatedResult
eval' command remainingArgs = case command of 
    "exit" -> Exit
    "echo" -> PrintAndContinue remainingArgs
    "type" -> PrintAndContinue $ handleTypeCommand remainingArgs
    _ -> PrintAndContinue $ command ++ ": command not found"

handleTypeCommand :: String -> String
handleTypeCommand remainingArgs = case remainingArgs of
    x | x `elem` ["exit", "echo", "type"] -> x ++ " is a shell builtin"
    _ -> remainingArgs <> ": not found"

getCommand :: String -> String
getCommand args = head (words args)

getRemainingArgs :: String -> String
getRemainingArgs args  = unwords (tail $ words args)

handleEval :: EvaluatedResult -> IO ()
handleEval evaluatedResult = case evaluatedResult of
    PrintAndContinue str -> printAndContinue str
    Continue -> main
    Exit -> pure ()

printAndContinue :: String -> IO ()
printAndContinue str = do
    putStrLn str
    hFlush stdout
    main