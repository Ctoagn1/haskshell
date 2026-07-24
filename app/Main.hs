module Main (main) where

import System.IO (hFlush, hPutStrLn, stdout, stderr, withFile, Handle, IOMode (WriteMode, AppendMode), NewlineMode (inputNL))
import System.Directory (findExecutable, getCurrentDirectory, getHomeDirectory, doesDirectoryExist, listDirectory, doesFileExist, Permissions (executable), getPermissions, doesPathExist)
import System.Process (proc, createProcess, std_out, std_err, waitForProcess, CreateProcess (cwd), StdStream (UseHandle))
import Control.Monad.IO.Class
import System.Posix.Terminal
import System.Posix.IO (stdInput)
import GHC.IO.Encoding (CodingProgress(OutputUnderflow))
import Data.List (isPrefixOf, nub, intercalate)
import System.FilePath ((</>), splitSearchPath)
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
handleCompletion word prev = do
    executables <- getExecutablesFromPATH
    let names = nub (builtinNames ++ executables)
    let matches = filter (word `isPrefixOf`) names
    case (matches, prev) of
        ([], _) -> do
            putChar '\x07'
            hFlush stdout
            loop word OtherKey
        ([one], _) -> do
            let current_length = length word
                to_put = drop current_length one
            putStr $ to_put ++ " "
            hFlush stdout
            loop (one ++ " ") OtherKey
        (_, OtherKey) -> do
            putChar '\x07'
            hFlush stdout
            let current_length = length word
                complete = longestCommonPrefix matches
                to_put = drop current_length complete
            putStr to_put
            hFlush stdout
            loop complete TabKey
        (_, TabKey) -> do
            putChar '\n'
            putStr $ intercalate "  " matches
            putChar '\n'
            putStr $ "$ " ++ word
            hFlush stdout
            loop word TabKey


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