module Main (main) where

import System.IO (hFlush, hPutStrLn, stdout, stderr, withFile, Handle, IOMode (WriteMode, AppendMode), NewlineMode (inputNL))
import System.Directory (findExecutable, getCurrentDirectory, getHomeDirectory, doesDirectoryExist, listDirectory, doesFileExist, Permissions (executable), getPermissions, doesPathExist, getDirectoryContents)
import System.Process (proc, createProcess, std_out, std_err, waitForProcess, CreateProcess (cwd), StdStream (UseHandle))
import Control.Monad.IO.Class
import System.Posix.Terminal
import System.Posix.IO (stdInput)
import GHC.IO.Encoding (CodingProgress(OutputUnderflow))
import Data.List (isPrefixOf, nub, intercalate, sort)
import System.FilePath ((</>), splitSearchPath, splitFileName)
import GHC.IO.Handle.Types (Handle__)
import GHC.IO.Handle.Internals (flushBuffer)
import System.Environment (lookupEnv)
import Control.Monad 



data KeyType = TabKey | OtherKey
main :: IO ()
main = do
    enableRawMode
    putStr "$ "
    hFlush stdout
    loop "" OtherKey



enableRawMode :: IO TerminalAttributes
enableRawMode = do
    old <- getTerminalAttributes stdInput 
    let raw = foldl withoutMode old [EnableEcho, ProcessInput, KeyboardInterrupts, StartStopOutput]
    setTerminalAttributes stdInput raw Immediately
    pure old
loop :: String -> KeyType -> IO ()
loop buf prev= do 
    ch <- getChar
    case ch of
        '\n' -> do
            if null buf then do 
                putStr "\n$ "
                hFlush stdout
                loop "" OtherKey 
            else do
                putChar '\n'
                continue <- runCommand(commandParse(tokenize buf))
                if continue then do
                    putStr "$ "
                    hFlush stdout
                    loop "" OtherKey
                else pure ()
        '\t' -> do 
            handleCompletion buf prev
            
        '\DEL' -> if null buf then loop buf OtherKey else do
            putStr "\b  \b\b"
            hFlush stdout
            loop (init buf) OtherKey
        _ -> do
            putChar ch
            hFlush stdout
            loop (buf ++ [ch]) OtherKey


handleCompletion :: String -> KeyType -> IO ()
handleCompletion input prev = do
    let allwords = splitKeepTrailing ' ' input
    case allwords of
        [] -> loop input OtherKey
        [command] -> do
            executables <- getExecutablesFromPATH
            let names = nub (builtinNames ++ executables)
            let matches = filter (input `isPrefixOf`) names
            case (matches, prev) of
                ([], _) -> do
                    putChar '\x07'
                    hFlush stdout
                    loop input OtherKey
                ([one], _) -> do
                    let current_length = length input
                        to_put = drop current_length one
                    putStr $ to_put ++ " "
                    hFlush stdout
                    loop (one ++ " ") OtherKey
                (_, OtherKey) -> do
                    putChar '\x07'
                    hFlush stdout
                    let current_length = length input
                        complete = longestCommonPrefix matches
                        to_put = drop current_length complete
                    putStr to_put
                    hFlush stdout
                    loop complete TabKey
                (_, TabKey) -> do
                    putChar '\n'
                    putStr $ intercalate "\t" (sort matches)
                    putChar '\n'
                    putStr $ "$ " ++ input
                    hFlush stdout
                    loop input TabKey
        _ -> do
            let wd = last allwords
            let (_, fileName) = splitFileName wd
            matches <- getCompletedFiles wd
            case (matches, prev) of
                ([], _) -> do
                    putChar '\x07'
                    hFlush stdout
                    loop input OtherKey
                ([one], _) -> do
                    let current_length = length fileName
                        to_put = drop current_length one

                    isD <- isDir  (wd ++ to_put)
                    if isD then 
                        putStr $ to_put ++ "/"
                    else
                        putStr $ to_put ++ " "
                    hFlush stdout
                    loop (input ++ to_put ++ " ") OtherKey
                (_, OtherKey) -> do
                    putChar '\x07'
                    hFlush stdout
                    let current_length = length wd
                        complete = longestCommonPrefix matches
                        to_put = drop current_length complete
                    putStr to_put
                    hFlush stdout
                    loop (input ++ to_put) TabKey
                (_, TabKey) -> do
                    putChar '\n'
                    putStr $ intercalate "\t" (sort matches)
                    putChar '\n'
                    putStr $ "$ " ++ input
                    hFlush stdout
                    loop input TabKey


isDir :: String -> IO Bool
isDir ('/':path) =
    doesDirectoryExist ('/':path)
isDir path = do
    cwd <- getCurrentDirectory
    doesDirectoryExist (cwd </> path)

getCompletedFiles :: String -> IO [FilePath]
getCompletedFiles ('/' : path) = do
    let (dir, file) = splitFileName ('/' : path) 
    dirExists <- doesDirectoryExist dir
    if dirExists then do
        files <- getDirectoryContents dir
        
        if null file then
            pure $ filter (\x -> '.' /= head x) files
        else 
            pure $ filter (file `isPrefixOf` ) files
    else
        pure []
getCompletedFiles path = do
    let (dir, file) = splitFileName path
    cwd <- getCurrentDirectory
    let newDir = cwd </> dir
    dirExists <- doesDirectoryExist newDir
    if dirExists then do
        files <- getDirectoryContents (cwd </> dir)
        if null file then
            pure $ filter (\x -> '.' /= head x) files
        else 
            pure $ filter (file `isPrefixOf` ) files
    else
        pure []
    
            
splitKeepTrailing :: Char -> String -> [String]
splitKeepTrailing c s =
    go s ""
  where
    go [] current = [current]
    go (x:xs) current
        | x == c    = current : go xs ""
        | otherwise = go xs (current ++ [x])


commonPrefix :: String -> String -> String
commonPrefix (x : xs) (y : ys)
  | x == y = x : commonPrefix xs ys
commonPrefix _ _ = ""

longestCommonPrefix :: [String] -> String
longestCommonPrefix = foldl1 commonPrefix



getExecutablesFromPATH :: IO [String]
getExecutablesFromPATH = do
    mpath <- lookupEnv "PATH"
    case mpath of
        Nothing -> pure []
        Just path -> do
            names <- mapM executablesInDir (splitSearchPath path)
            pure . nub . concat $ names

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
  ["exit", "echo", "type", "pwd"]


data TokenState = Normal | SingleQuote | DoubleQuote | Backslash TokenState
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
                        '"' -> go cs current tokens DoubleQuote
                        '\\' -> go cs current tokens (Backslash state)
                        ' ' -> go cs "" (if null current then tokens else tokens ++ [current]) Normal
                        _ -> go cs (current ++ [c]) tokens Normal
                SingleQuote ->
                    case c of
                        '\'' -> go cs current tokens Normal
                        _ -> go cs (current ++ [c]) tokens SingleQuote
                DoubleQuote ->
                    case c of 
                        '"' -> go cs current tokens Normal
                        '\\' -> go cs current tokens (Backslash state)
                        _ -> go cs (current ++ [c]) tokens DoubleQuote
                Backslash last_state ->
                    go cs (current ++ [c]) tokens last_state

handleTypeCommand :: String -> IO String
handleTypeCommand remainingArgs = case remainingArgs of
    x | x `elem` ["exit", "echo", "type"] -> pure $ x ++ " is a shell builtin"
    _ -> do 
        result <- findExecutable remainingArgs
        case result of
            Just path -> pure $ remainingArgs ++ " is " ++ path
            Nothing ->  pure $ remainingArgs ++ ": not found"


data Command = Command {cmd :: String, args :: [String], redirect :: Maybe (Redirect, String)}
data Redirect = OutWrite | OutAppend | ErrWrite| ErrAppend 
data ParseMode = Redir Redirect | Norm
data ParseError = SyntaxError
commandParse :: [String] -> Command
commandParse input = 
    
    go input [] Nothing Norm
    where 
        go [] args redirect _ = Command {cmd = head args, args = tail args, redirect = redirect}
        go (x:xs) args redirect mode = 
            case mode of 
                Norm ->
                    case x of 
                        "1>" -> go xs args redirect (Redir OutWrite) 
                        ">" -> go xs args redirect (Redir OutWrite) 
                        ">>" -> go xs args redirect (Redir OutAppend) 
                        "1>>" -> go xs args redirect (Redir OutAppend) 
                        "2>" -> go xs args redirect (Redir ErrWrite) 
                        "2>>" -> go xs args redirect (Redir ErrAppend) 
                        _ -> go xs (args ++ [x]) redirect Norm
                Redir mode -> go xs args (Just (mode, x)) Norm

runCommand :: Command -> IO Bool
runCommand command =
    case redirect command of
        Nothing -> runCommandWith stdout stderr command
        Just (OutWrite, t) -> do
            path <- expandHome t
            withFile path WriteMode $ \h ->
                runCommandWith h stderr command
        Just (ErrWrite, t) -> do
            path <- expandHome t
            withFile path WriteMode $ \h ->
                runCommandWith stdout h command
        Just (OutAppend, t) -> do
            path <- expandHome t
            withFile path AppendMode $ \h ->
                runCommandWith h stderr command
        Just (ErrAppend, t) -> do
            path <- expandHome t
            withFile path AppendMode $ \h ->
                runCommandWith stdout h command
runCommandWith :: Handle -> Handle -> Command -> IO Bool
runCommandWith out err command = 
    case cmd command of
        "exit" ->
            pure False
        "pwd" -> do
            cwd <- getCurrentDirectory
            hPutStrLn out cwd
            pure True
        "echo" -> do
            hPutStrLn out (unwords (args command))
            pure True
        "type" -> do
            let arg = unwords (args command)
            if isBuiltin arg
                then hPutStrLn out $ arg ++ " is a shell builtin"
                else do
                result <- findExecutable arg
                case result of
                    Just fullPath ->
                        hPutStrLn out $ arg ++ " is " ++ fullPath
                    Nothing ->
                        hPutStrLn out $ arg ++ ": not found"
            pure True
        _ -> do
            result <- findExecutable (cmd command)
            case result of
                Just fullPath -> do
                    (_, _, _, processHandle) <-
                        createProcess
                        (proc (cmd command) (args command))
                            { std_out = UseHandle out,
                            std_err = UseHandle err
                            }
                    _ <- waitForProcess processHandle
                    pure ()
                Nothing ->
                    hPutStrLn out $ cmd command ++ ": command not found"
            pure True



expandHome :: FilePath -> IO FilePath
expandHome path
  | path == "~" =
      getHomeDirectory
  | "~/" `isPrefixOf` path = do
      home <- getHomeDirectory
      pure (home </> drop 2 path)
  | otherwise =
      pure path