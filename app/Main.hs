module Main (main) where

import System.IO (hFlush, hPutStrLn, stdout, stderr, withFile, Handle, IOMode (WriteMode, AppendMode), NewlineMode (inputNL))
import System.Directory (findExecutable, getCurrentDirectory, getHomeDirectory, doesDirectoryExist, listDirectory, doesFileExist, Permissions (executable), getPermissions, doesPathExist, getDirectoryContents, setCurrentDirectory)
import System.Process (proc, createProcess, std_out, std_err, waitForProcess, CreateProcess (cwd), StdStream (UseHandle), readProcess, ProcessHandle, getPid)
import Control.Monad.IO.Class
import System.Posix.Terminal
import System.Posix.IO (stdInput)
import GHC.IO.Encoding (CodingProgress(OutputUnderflow))
import Data.List (isPrefixOf, nub, intercalate, sort)
import qualified Data.Map as Map
import System.FilePath ((</>), splitSearchPath, splitFileName)
import GHC.IO.Handle.Types (Handle__)
import GHC.IO.Handle.Internals (flushBuffer)
import System.Environment (lookupEnv, setEnv)
import Control.Monad 
import Data.Maybe (isJust)
import System.Process.Internals (ProcessHandle__)

data ProcStatus = Running 
instance Show ProcStatus where
    show Running = "Running" ++ replicate 17 ' '
data ProcInfo = ProcInfo {handle :: ProcessHandle, name :: String, status :: ProcStatus }
data ShellState = ShellState {completions :: Map.Map String FilePath, bgJobs :: Map.Map Int ProcInfo, nextJobId :: Int, latestJobId :: Int, secondLatestJobId :: Int}

data KeyType = TabKey | OtherKey
main :: IO ()
main = do
    enableRawMode
    putStr "$ "
    hFlush stdout
    loop "" OtherKey ShellState {completions = Map.empty, bgJobs = Map.empty, nextJobId = 1, latestJobId = 0, secondLatestJobId = 0}



enableRawMode :: IO TerminalAttributes
enableRawMode = do
    old <- getTerminalAttributes stdInput 
    let raw = foldl withoutMode old [EnableEcho, ProcessInput, KeyboardInterrupts, StartStopOutput]
    setTerminalAttributes stdInput raw Immediately
    pure old
loop :: String -> KeyType -> ShellState -> IO ()
loop buf prev state = do 
    ch <- getChar
    case ch of
        '\n' -> do
            if null buf then do 
                putStr "\n$ "
                hFlush stdout
                loop "" OtherKey state
            else do
                putChar '\n'
                (continue, nstate) <- runCommand (commandParse (tokenize buf)) state
                if continue then do
                    putStr "$ "
                    hFlush stdout
                    loop "" OtherKey nstate
                else pure ()
        '\t' -> do 
            handleCompletion buf prev state
            
        '\DEL' -> if null buf then loop buf OtherKey state else do
            putStr "\b  \b\b"
            hFlush stdout
            loop (init buf) OtherKey state 
        _ -> do
            putChar ch
            hFlush stdout
            loop (buf ++ [ch]) OtherKey state

replaceToken :: String -> String -> IO ()
replaceToken old new = do
    putStr (replicate (length old) '\b')
    putStr (replicate (length old) ' ')
    putStr (replicate (length old) '\b')
    putStr new
    hFlush stdout
    
handleCompletion :: String -> KeyType -> ShellState -> IO ()
handleCompletion input prev state = do
    let wds=  words input
    let allwords = if last input == ' ' then wds ++ [""] else wds
    case allwords of
        [] -> loop input OtherKey state
        [command] -> do
            executables <- getExecutablesFromPATH
            let names = nub (builtinNames ++ executables)
            let matches = filter (input `isPrefixOf`) names
            case (matches, prev) of
                ([], _) -> do
                    putChar '\x07'
                    hFlush stdout
                    loop input OtherKey state
                ([one], _) -> do
                    replaceToken command (one ++ " ")
                    loop (one ++ " ") OtherKey state
                (_, OtherKey) -> do
                    putChar '\x07'
                    hFlush stdout
                    let complete = longestCommonPrefix matches
                    replaceToken command (longestCommonPrefix matches)
                    loop complete TabKey state
                (_, TabKey) -> do
                    putChar '\n'
                    putStr $ intercalate "\t" (sort matches)
                    putChar '\n'
                    putStr $ "$ " ++ input
                    hFlush stdout
                    loop input TabKey state
        _ -> do

            let wd = last allwords
            let pre_wd = last (init allwords)
            
            let (_, fileName) = splitFileName wd
            setEnv "COMP_LINE" input
            setEnv "COMP_POINT" $ show (length input)
            matches <- case getCompletions allwords Nothing state of
                    Nothing -> getCompletedFiles wd
                    Just (str, path) -> getCompletionOutput path str wd pre_wd


            case (matches, prev) of
                ([], _) -> do
                    putChar '\x07'
                    hFlush stdout
                    loop input OtherKey state
                ([one], _) -> do
                    let current_length = length fileName
                        to_put = drop current_length one
                    let ch = if last one == '/' then "" else " "
                    putStr $ to_put ++ ch
                    hFlush stdout
                    loop (input ++ to_put ++ ch) OtherKey state
                (_, OtherKey) -> do
                    putChar '\x07'
                    hFlush stdout
                    let current_length = length wd
                        complete = longestCommonPrefix matches
                        to_put = drop current_length complete
                    putStr to_put
                    hFlush stdout
                    loop (input ++ to_put) TabKey state
                (_, TabKey) -> do
                    putChar '\n'
                    putStr $ intercalate "\t" (sort matches)
                    putChar '\n'
                    putStr $ "$ " ++ input
                    hFlush stdout
                    loop input TabKey state

getCompletions :: [String] -> Maybe (String, FilePath) -> ShellState -> Maybe (String, FilePath)
getCompletions [] x _ = x
getCompletions (x:xs) cur c =
    case Map.lookup x (completions c) of
        Just y -> getCompletions xs (pure (x, y)) c
        Nothing -> getCompletions xs cur c

getCompletionOutput :: FilePath -> String -> String -> String -> IO [String]
getCompletionOutput path comp cur prev = do
    output <- readProcess path [comp, cur, prev] ""
    pure $ lines output


isDir :: String -> IO Bool
isDir ('/':path) =
    doesDirectoryExist ('/':path)
isDir path = do
    cwd <- getCurrentDirectory
    doesDirectoryExist (cwd </> path)


getCompletedFiles :: String -> IO [FilePath]
getCompletedFiles path = do
    let (dir, file) = splitFileName path
    newDir <- resolvePath dir
    dirExists <- doesDirectoryExist newDir
    if dirExists then do
        files <- getDirectoryContents newDir
        if null file then do
            let filt = filter (\x -> '.' /= head x) files
            mapM (\x -> do
                isD <- isDir (dir </> x)
                pure $ if isD then x ++ "/" else x) filt
        else do
            let filt =  filter (file `isPrefixOf` ) files
            mapM (\x -> do
                isD <- isDir (dir </> x)
                pure $ if isD then x ++ "/" else x) filt
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

resolvePath :: FilePath -> IO FilePath
resolvePath path 
    | "/" `isPrefixOf` path = pure path
    | "~" `isPrefixOf` path = do
            hd <- getHomeDirectory
            pure (hd </> drop 2 path)
    | otherwise = do
            cwd <- getCurrentDirectory
            pure (cwd </> path)

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
  ["exit", "echo", "type", "pwd", "complete", "cd", "jobs"]


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

data Command = Command {cmd :: String, args :: [String], redirect :: Maybe (Redirect, String), isBgJob :: Bool}
data Redirect = OutWrite | OutAppend | ErrWrite| ErrAppend 
data ParseMode = Redir Redirect | Norm | Bg
commandParse :: [String] -> Command
commandParse input = 
    
    go input [] Nothing Norm
    where 
        go [] args redirect _ = Command {cmd = head args, args = tail args, redirect = redirect, isBgJob = False}
        go ["&"] args redirect Norm  = Command {cmd = head args, args = tail args, redirect = redirect, isBgJob = True}
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

runCommand :: Command -> ShellState -> IO (Bool, ShellState)
runCommand command state=
    case redirect command of
        Nothing -> runCommandWith stdout stderr command state
        Just (OutWrite, t) -> do
            path <- resolvePath t
            withFile path WriteMode $ \h ->
                runCommandWith h stderr command state
        Just (ErrWrite, t) -> do
            path <- resolvePath t
            withFile path WriteMode $ \h ->
                runCommandWith stdout h command state
        Just (OutAppend, t) -> do
            path <- resolvePath t
            withFile path AppendMode $ \h ->
                runCommandWith h stderr command state
        Just (ErrAppend, t) -> do
            path <- resolvePath t
            withFile path AppendMode $ \h ->
                runCommandWith stdout h command state
                
runCommandWith :: Handle -> Handle -> Command -> ShellState -> IO (Bool, ShellState)
runCommandWith out err command state = 
    case cmd command of
        "exit" ->
            pure (False, state)
        "pwd" -> do
            cwd <- getCurrentDirectory
            hPutStrLn out cwd
            pure (True, state)
        "cd" -> do
            let arg = unwords (args command)
            newCwd <- resolvePath arg
            isDir <- doesDirectoryExist newCwd
            if isDir then do
                setCurrentDirectory newCwd
                pure (True, state)
            else do
                hPutStrLn out ("cd: " ++ arg ++ ": No such file or directory")
                pure (True, state)

        "echo" -> do
            hPutStrLn out (unwords (args command))
            pure (True, state)
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
            pure (True, state)
        "complete" -> do
            (str, nstate) <- completeFunc (args command) Awaiting state
            if null str then pure 
                (True, nstate)
            else do
                hPutStrLn out str
                pure (True, nstate)
        "jobs" -> do
            let jobList = Map.toList (bgJobs state)
            let addPlus num = if num == latestJobId state then "+" else 
                    if num == secondLatestJobId state then "-" else ""
            mapM_ (\(jobNum, procState) -> 
                hPutStrLn out $ "[" ++ show jobNum ++ "]" ++ addPlus jobNum ++ "  " ++ show (status procState) ++ name procState ) jobList
            pure (True, state)
        _ -> do
            result <- findExecutable (cmd command)
            state' <- case result of
                Just fullPath -> do
                    (_, _, _, processHandle) <-
                        createProcess
                        (proc (cmd command) (args command))
                            { std_out = UseHandle out,
                            std_err = UseHandle err
                            }
                    if isBgJob command then do
                        pid <- getPid processHandle
                        let osPid = maybe "unknown" show pid
                            j_id = nextJobId state
                            procInf = ProcInfo {name = cmd command ++ " " ++ unwords (args command), handle = processHandle, status = Running}
                            state' = state {bgJobs = Map.insert j_id procInf (bgJobs state), nextJobId = j_id + 1, secondLatestJobId = latestJobId state, latestJobId = j_id}
                        putStrLn $ "[" ++ show j_id ++ "] " ++ osPid
                        pure state'
                    else do
                        _ <- waitForProcess processHandle
                        let state' = state
                        pure state
                Nothing -> do
                    hPutStrLn out $ cmd command ++ ": command not found"
                    pure state
            pure (True, state')

data CompleteMode = Awaiting | Print | AddPath | AddName String | Remove
completeFunc :: [String] -> CompleteMode -> ShellState -> IO (String, ShellState)
completeFunc [] _ c = pure ("complete: not enough args provided", c)
completeFunc (arg:args) Awaiting c = 
    case arg of
        "-p" -> completeFunc args Print c
        "-C" -> completeFunc args AddPath c
        "-r" -> completeFunc args Remove c
        _ -> pure ("complete: " ++ arg ++ ": invalid arg", c)
completeFunc (arg:args) Print c =
    let x = Map.lookup arg (completions c) in
        case x of
            Nothing -> pure ("complete: " ++ arg ++ ": no completion specification", c)
            Just y -> pure ("complete -C \'" ++ y ++ "\' " ++ arg, c )
completeFunc (arg:args) AddPath c = do
    fullpath <- resolvePath arg
    completeFunc args (AddName fullpath) c 
completeFunc (arg:args) (AddName path) c =
    let c' = c {completions = Map.insert arg path (completions c)} in
        pure ("", c')
completeFunc (arg:args) Remove c =
    let c' = c {completions = Map.delete arg (completions c)} in
        pure ("", c')
