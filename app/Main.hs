module Main (main) where

import System.IO (hFlush, stdout)
import System.Directory (findExecutable)
import System.Process (callProcess)


data EvaluatedResult = PrintAndContinue String | Exit | Continue | Execute FilePath [String]

main :: IO ()
main = do
    putStr "$ "
    hFlush stdout
    cmd <- getLine
    evald_cmd <- eval cmd
    handleEval evald_cmd
    
eval :: String -> IO EvaluatedResult
eval args = if null args then pure Continue else eval' (getCommand args) (getRemainingArgs args)


eval' :: String -> String -> IO EvaluatedResult
eval' command remainingArgs = case command of 
    "exit" -> pure Exit
    "echo" -> pure $ PrintAndContinue remainingArgs
    "type" -> do
        result <- handleTypeCommand remainingArgs
        pure $ PrintAndContinue result
    _ -> do

        isExec <- findExecutable command
        case isExec of 
            Just fp -> pure $ Execute fp $ words remainingArgs
            Nothing -> pure $ PrintAndContinue $ command ++ ": command not found"


handleTypeCommand :: String -> IO String
handleTypeCommand remainingArgs = case remainingArgs of
    x | x `elem` ["exit", "echo", "type"] -> pure $ x ++ " is a shell builtin"
    _ -> do 
        result <- findExecutable remainingArgs
        case result of
            Just path -> pure $ remainingArgs ++ " is " ++ path
            Nothing ->  pure $ remainingArgs ++ ": not found"

getCommand :: String -> String
getCommand args = head (words args)

getRemainingArgs :: String -> String
getRemainingArgs args  = unwords (tail $ words args)

handleEval :: EvaluatedResult -> IO ()
handleEval evaluatedResult = case evaluatedResult of
    PrintAndContinue str -> printAndContinue str
    Continue -> main
    Exit -> pure ()
    Execute fp args -> do
        callProcess fp args

printAndContinue :: String -> IO ()
printAndContinue str = do
    putStrLn str
    hFlush stdout
    main