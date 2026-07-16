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

eval args = 
    let tokens = tokenize args
    in if null tokens then pure Continue else eval' (head tokens) (tail tokens)


eval' :: String -> [String] -> IO EvaluatedResult
eval' command remainingArgs = case command of 
    "exit" -> pure Exit
    "echo" -> pure $ PrintAndContinue $ unwords remainingArgs
    "type" -> do
        result <- handleTypeCommand $ unwords remainingArgs
        pure $ PrintAndContinue result
    _ -> do

        isExec <- findExecutable command
        case isExec of 
            Just fp -> pure $ Execute fp (command : remainingArgs)
            Nothing -> pure $ PrintAndContinue $ command ++ ": command not found"


data TokenState = Normal | SingleQuote | DoubleQuote
tokenize :: String -> [String]
tokenize path = 
    go path "" [] Normal
    where 
        go [] current tokens state
            | null current = tokens
            | otherwise = tokens ++ [current]

        go (c:cs) current tokens state =
            case state of
                Normal ->
                    case c of
                        '\'' -> go cs current tokens SingleQuote
                        --'"' -> go cs current tokens DoubleQuote
                        ' ' -> go cs "" (if null current then tokens else tokens ++ [current]) Normal
                        _ -> go cs (current ++ [c]) tokens Normal
                SingleQuote ->
                    case c of
                        '\'' -> go cs current tokens Normal
                        _ -> go cs (current ++ [c]) tokens SingleQuote
                DoubleQuote ->
                    case c of 
                        '"' -> go cs current tokens Normal
                        _ -> go cs (current ++ [c]) tokens DoubleQuote

handleTypeCommand :: String -> IO String
handleTypeCommand remainingArgs = case remainingArgs of
    x | x `elem` ["exit", "echo", "type"] -> pure $ x ++ " is a shell builtin"
    _ -> do 
        result <- findExecutable remainingArgs
        case result of
            Just path -> pure $ remainingArgs ++ " is " ++ path
            Nothing ->  pure $ remainingArgs ++ ": not found"

handleEval :: EvaluatedResult -> IO ()
handleEval evaluatedResult = case evaluatedResult of
    PrintAndContinue str -> printAndContinue str
    Continue -> main
    Exit -> pure ()
    Execute fp (first : args) -> do
        callProcess first args
        main

printAndContinue :: String -> IO ()
printAndContinue str = do
    putStrLn str
    hFlush stdout
    main