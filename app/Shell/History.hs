module Shell.History where

import Shell.Types
import Text.Read (readMaybe)
import Shell.Repl
import Control.Exception (try)
import System.Directory (doesFileExist)
import Shell.Parsing (resolvePath)
import System.Environment (lookupEnv)
    
getHistory :: [String] ->  HistoryMode -> ShellState -> IO (String, ShellState)
getHistory [] HistoryNormal state = pure (showHistory (history state) 1, state)
getHistory [] _ state = pure ("history: not enough arguments provided", state)
getHistory (x:xs) HistoryNormal state =
    case x of
        "-r" -> getHistory xs HistoryRead state
        "-w" -> getHistory xs HistoryWrite state
        "-a" -> getHistory xs HistoryAppend state
        _ -> case readMaybe x :: Maybe Int  of
                Nothing -> pure ("history: " ++ x ++ ": unrecognized argument", state)
                Just y -> pure (showHistory (drop (max 0 (length (history state) - y)) (history state)) (max 1 (length (history state) - y) ), state)
getHistory (x:xs) HistoryRead state = do
    path <- resolvePath x
    fExists <- doesFileExist path
    if fExists then do
        contents <- readFile path
        let newHistory = history state ++ lines contents
            s = state {history = newHistory, unappendedHistoryIndex = length newHistory }
        pure ("", s)
    else
        pure ("history: " ++ x ++ ": no such file", state)
getHistory (x:xs) HistoryWrite state = do
    result <- (try $ writeFile x (unlines (history state)) :: IO (Either IOError ()))
    case result of
        Left _ -> pure ("history: " ++ x ++ ": could not write to file", state)
        Right _ -> pure ("", state)
getHistory (x:xs) HistoryAppend state = do
    result <- try (appendFile x (unlines $ drop (unappendedHistoryIndex state) (history state))) :: IO (Either IOError ())

    case result of
        Left _ -> pure ("history: " ++ x ++ ": could not write to file", state)
        Right _ -> do 
            let s = state {unappendedHistoryIndex = length (history state) }
            pure ("", s)

showHistory :: [String] -> Int -> String
showHistory [] i = ""
showHistory (x:xs) i = "    " ++ show i ++ "  " ++ x ++ "\n" ++ showHistory xs (i+1)

initializeHistory :: ShellState -> IO ShellState
initializeHistory state = do
    histPath <- lookupEnv "HISTFILE"
    case histPath of
        Nothing -> pure state
        Just path -> do
            hist <- try (readFile path) :: IO (Either IOError String)
            case hist of
                Left _ -> pure state
                Right text -> pure state {history = lines text, unappendedHistoryIndex = length (lines text)}

saveHistory :: ShellState -> IO ()
saveHistory state = do
    histfile <- lookupEnv "HISTFILE"
    case histfile of
        Just f -> appendFile f (unlines $ drop (unappendedHistoryIndex state) (history state))
        Nothing -> pure ()
