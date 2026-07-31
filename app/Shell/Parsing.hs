module Shell.Parsing where
import Shell.Types
import System.Directory (getHomeDirectory, getCurrentDirectory, doesDirectoryExist, getPermissions, Permissions (executable), doesFileExist, listDirectory)
import System.FilePath ((</>))
import Data.List (isPrefixOf)
import Control.Monad (filterM)

tokenize :: String -> [(String, Bool)]
tokenize path = 
    go path "" [] (Normal False)
    where 
        go [] current tokens state
            | null current = tokens
            | otherwise = tokens ++ [(current, case state of
                Normal x -> x
                _ -> False)]

        go (c:cs) current tokens state =
            case state of
                Normal lastQuoted->
                    case c of
                        '\'' -> go cs current tokens SingleQuote 
                        '"' -> go cs current tokens DoubleQuote 
                        '\\' -> go cs current tokens (Backslash state) 
                        ' ' -> go cs "" (if null current then tokens else tokens ++ [(current, lastQuoted)]) (Normal False)
                        _ -> go cs (current ++ [c]) tokens (Normal False)
                SingleQuote ->
                    case c of
                        '\'' -> go cs current tokens (Normal True)
                        _ -> go cs (current ++ [c]) tokens SingleQuote
                DoubleQuote ->
                    case c of 
                        '"' -> go cs current tokens (Normal True) 
                        '\\' -> go cs current tokens (Backslash state)
                        _ -> go cs (current ++ [c]) tokens DoubleQuote
                Backslash last_state ->
                    go cs (current ++ [c]) tokens last_state


parsePipeline :: [(String, Bool)] -> [Command]
parsePipeline toks = do
    let c =  splitOn ("|", False) toks
    map commandParse c
        
commandParse :: [(String, Bool)] -> Command
commandParse input = 
    go input [] Nothing Norm
    where 
        go [] [] r _ = Command {cmd = "", args = [], redirect = r, isBgJob = False }
        go [] args redirect _ = Command {cmd = head args, args = tail args, redirect = redirect, isBgJob = False}
        go [("&", False)] args redirect Norm  = Command {cmd = head args, args = tail args, redirect = redirect, isBgJob = True}
        go (x:xs) args redirect mode = 
            case mode of 
                Norm ->
                    case x of 
                        ("1>", False) -> go xs args redirect (Redir OutWrite) 
                        (">", False) -> go xs args redirect (Redir OutWrite) 
                        (">>", False) -> go xs args redirect (Redir OutAppend) 
                        ("1>>", False) -> go xs args redirect (Redir OutAppend) 
                        ("2>", False) -> go xs args redirect (Redir ErrWrite) 
                        ("2>>", False) -> go xs args redirect (Redir ErrAppend) 
                        _ -> go xs (args ++ [fst x]) redirect Norm
                Redir mode -> go xs args (Just (mode, fst x)) Norm

isDir :: String -> IO Bool
isDir ('/':path) =
    doesDirectoryExist ('/':path)
isDir path = do
    cwd <- getCurrentDirectory
    doesDirectoryExist (cwd </> path)

        
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn sep xs = case break (== sep) xs of
  (chunk, []) -> [chunk]
  (chunk, _ : rest) -> chunk : splitOn sep rest

splitKeepTrailing :: Char -> String -> [String]
splitKeepTrailing c s =
    go s ""
  where
    go [] current = [current]
    go (x:xs) current
        | x == c    = current : go xs ""
        | otherwise = go xs (current ++ [x])



resolvePath :: FilePath -> IO FilePath
resolvePath path 
    | "/" `isPrefixOf` path = pure path
    | "~" `isPrefixOf` path = do
            hd <- getHomeDirectory
            pure (hd </> drop 2 path)
    | otherwise = do
            cwd <- getCurrentDirectory
            pure (cwd </> path)


executablesInDir :: FilePath -> IO [FilePath]
executablesInDir dir = do
    exists <- doesDirectoryExist dir
    if not exists then pure []
        else do
            files <- listDirectory dir
            filterM (isExecutable . (dir </>)) files

isExecutable :: FilePath -> IO Bool
isExecutable path = do
    isFile <- doesFileExist path
    if not isFile
        then pure False
        else executable <$> getPermissions path

isBuiltin :: String -> Bool
isBuiltin cmd =
  cmd `elem` builtinNames

builtinNames =
  ["exit", "echo", "type", "pwd", "complete", "cd", "jobs", "history", "declare"]

