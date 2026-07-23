module Main (main) where

import System.IO (hFlush, hPutStrLn, stdout, stderr, withFile, Handle, IOMode (WriteMode, AppendMode))
import System.Directory (findExecutable, getCurrentDirectory, getHomeDirectory, doesDirectoryExist, listDirectory, doesFileExist, Permissions (executable), getPermissions)
import System.Process (proc, createProcess, std_out, std_err, waitForProcess, CreateProcess (cwd), StdStream (UseHandle))
import System.Console.Haskeline (Completion (Completion, replacement, display, isFinished), Settings, InputT, runInputT, getInputLine, complete, defaultSettings, CompletionFunc, completeWord, simpleCompletion)
import Control.Monad.IO.Class
import GHC.IO.Encoding (CodingProgress(OutputUnderflow))
import Data.List (isPrefixOf, nub)
import System.FilePath ((</>), splitSearchPath)
import GHC.IO.Handle.Types (Handle__)
import GHC.IO.Handle.Internals (flushBuffer)
import System.Environment (lookupEnv)
import Control.Monad 

settings :: Settings IO
settings =
    (defaultSettings :: Settings IO)
        { complete = completion
        }

main :: IO ()
main = runInputT settings loop

loop :: InputT IO ()
loop = do 
    minput <- getInputLine "$ "
    case minput of 
        Nothing -> pure ()
        Just line -> do
            let cmd = commandParse (tokenize line) 
            continue <- liftIO (runCommand cmd)
            if continue then loop else pure ()

completion :: CompletionFunc IO
completion = completeWord Nothing " " $ \word -> do
    executables <- liftIO getExecutablesFromPATH
    let names = builtinNames ++ executables
    pure [Completion
            {replacement = if word == name then name else name ++ " ", 
            display = name, 
            isFinished = True
            } 
        | name <- filter (word `isPrefixOf`) names
        ]

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



printAndContinue :: String -> IO ()
printAndContinue str = do
    putStrLn str
    hFlush stdout
    main

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