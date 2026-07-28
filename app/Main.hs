module Main (main) where

import System.IO (hFlush, hPutStrLn, stdout, stderr, withFile, Handle, IOMode (WriteMode, AppendMode), NewlineMode (inputNL), stdin, hReady, hPutStr)
import System.Directory (findExecutable, getCurrentDirectory, getHomeDirectory, doesDirectoryExist, listDirectory, doesFileExist, Permissions (executable), getPermissions, doesPathExist, getDirectoryContents, setCurrentDirectory)
import System.Process (proc, createProcess, std_out, std_err, waitForProcess, CreateProcess (cwd, std_in), StdStream (UseHandle, Inherit), readProcess, ProcessHandle, getPid, getProcessExitCode)
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
import Text.Read (readMaybe)
import System.Directory.Internal.Prelude (catchIOError, try, isAlpha)
import GHC.IO.Exception (IOException(IOError))
import Data.Char (isAlphaNum)

data ProcStatus = Running | Done deriving (Eq)
instance Show ProcStatus where
    show Running = "Running" ++ replicate 17 ' '
    show Done = "Done" ++ replicate 20 ' '
data ProcInfo = ProcInfo {handle :: ProcessHandle, name :: String, status :: ProcStatus }
data ShellState = ShellState {completions :: Map.Map String FilePath, bgJobs :: Map.Map Int ProcInfo, nextJobId :: Int, latestJobIds :: [Int], history :: [String], historyPosition :: Int, unappendedHistoryIndex :: Int, declares :: Map.Map String String }

data KeyType = TabKey | OtherKey
main :: IO ()
main = do
    enableRawMode
    putStr "$ "
    let init = ShellState {completions = Map.empty, bgJobs = Map.empty, nextJobId = 1, latestJobIds = [], history = [], historyPosition = 0, unappendedHistoryIndex = 0, declares = Map.empty} 
    state <- initializeHistory init
    hFlush stdout
    loop "" OtherKey state

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
                state' <- reapJobs DoneOnly state stdout
                putStr "\n$ "
                hFlush stdout
                loop "" OtherKey state'
            else do
                putChar '\n'
                let s = state {history = history state ++ [buf], historyPosition = length (history state) + 1}
                (continue, nstate) <- runCommand (commandParse $ substituteVars (tokenize buf) s) s
                if continue then do
                    state' <- reapJobs DoneOnly nstate stdout 
                    putStr "$ "
                    hFlush stdout
                    loop "" OtherKey state'
                else do
                    saveHistory nstate
                    pure ()
        '\t' -> do 
            handleCompletion buf prev state
            
        '\DEL' -> if null buf then loop buf OtherKey state else do
            putStr "\b  \b\b"
            hFlush stdout
            loop (init buf) OtherKey state 
        '\ESC' -> do
            is_an_arrow <- hReady stdin
            when is_an_arrow $ do
                c2 <- getChar
                c3 <- getChar
                case (c2, c3) of
                    ('[', 'A' ) -> handleUpArrow buf state
                    ('[', 'B') -> handleDownArrow buf state
                    _ -> loop buf OtherKey state
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

handleUpArrow :: String -> ShellState -> IO ()
handleUpArrow buf state = do
    let newpos = max 0 (historyPosition state - 1)
        s = state {historyPosition = newpos}
    if newpos < length (history state) then do
        let new = history state !! newpos
        replaceToken buf new
        loop new OtherKey s
    else do
        replaceToken buf ""
        loop "" OtherKey s

handleDownArrow :: String -> ShellState -> IO ()
handleDownArrow buf state = do
    let newpos = min (length (history state)) (historyPosition state + 1)
        s = state {historyPosition = newpos}
    if newpos < length (history state) then do
        let new = history state !! newpos
        replaceToken buf new
        loop new OtherKey s
    else do
        replaceToken buf ""
        loop "" OtherKey s
    
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

declareVar :: ShellState -> [String] -> (String, ShellState)
declareVar state []  = ("declare: not enough args provided", state)
declareVar state ("-p":xs) = do
    if null xs then ("declare: not enough args provided", state) else do
        let var = head xs
            entry = Map.lookup var (declares state)
        case entry of
            Nothing -> ("declare: " ++ var ++ ": not found", state)
            Just x -> ("declare -- " ++ var ++ "=\"" ++ x ++ "\"", state)
declareVar state (x:xs) = do
    let inp = splitOn '=' x
    if length inp /= 2 || not (validIdentifier $ head inp) then 
        ("declare: `" ++ x ++ "': not a valid identifier", state) 
    else
        ("", state {declares = Map.insert (head inp) (last inp) (declares state)})


validIdentifier :: String -> Bool
validIdentifier [] = False
validIdentifier (x:xs) =
    (isAlpha x || x == '_') && all (\x -> isAlphaNum x || x == '_') xs


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
  ["exit", "echo", "type", "pwd", "complete", "cd", "jobs", "history", "declare"]

data SubMode = SubNorm | SubDollar | SubOpenBrace
substituteVars :: [String] -> ShellState -> [String]
substituteVars inp state = filter (not . null) (map (\x ->varSub x "" "" state SubNorm) inp)

varSub :: String -> String -> String -> ShellState -> SubMode -> String
varSub [] acc var_acc state _ = do
    let v = case Map.lookup var_acc (declares state) of
            Nothing -> ""
            Just x -> x 
    acc ++ v

varSub (x:xs) acc var_acc state SubNorm = 
    case x of
        '$' -> do 
            if not (null xs) && head xs == '{' then
                varSub (drop 1 xs) acc var_acc state SubOpenBrace
            else
                varSub xs acc var_acc state SubDollar
        _ -> varSub xs (acc ++ [x]) var_acc state SubNorm
varSub (x:xs) acc var_acc state SubDollar =
    case x of
        '$' -> do
            let v = case Map.lookup var_acc (declares state) of
                    Nothing -> ""
                    Just y -> y
            if not (null xs) && head xs == '{' then
                varSub (tail xs) (acc ++ [x]) "" state SubOpenBrace
            else
                varSub xs (acc ++ [x]) "" state SubDollar
        _ -> do
            if isAlphaNum x || x == '_' then 
                varSub xs acc (var_acc ++ [x]) state SubDollar
            else do             
                let v = case Map.lookup var_acc (declares state) of
                        Nothing -> ""
                        Just y -> y
                varSub xs (acc ++ v) "" state SubNorm
varSub (x:xs) acc var_acc state SubOpenBrace = 
    case x of
        '}' -> do
            let v = case Map.lookup var_acc (declares state) of
                    Nothing -> ""
                    Just y -> y
            varSub xs (acc ++ v) "" state SubNorm
        _ -> varSub xs acc (var_acc ++ [x]) state SubOpenBrace
        

            




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

{-}
pipeline :: [[String]] -> IO (Bool, ShellState)
pipeline input = do
    let commandList = map commandParse input
-}
runCommand :: Command -> ShellState -> IO (Bool, ShellState)
runCommand command state=
    case redirect command of
        Nothing -> runCommandWith stdin stdout stderr command state
        Just (OutWrite, t) -> do
            path <- resolvePath t
            withFile path WriteMode $ \h ->
                runCommandWith stdin h stderr command state
        Just (ErrWrite, t) -> do
            path <- resolvePath t
            withFile path WriteMode $ \h ->
                runCommandWith stdin stdout h command state
        Just (OutAppend, t) -> do
            path <- resolvePath t
            withFile path AppendMode $ \h ->
                runCommandWith stdin h stderr command state
        Just (ErrAppend, t) -> do
            path <- resolvePath t
            withFile path AppendMode $ \h ->
                runCommandWith stdin stdout h command state
                
runCommandWith :: Handle -> Handle -> Handle -> Command -> ShellState -> IO (Bool, ShellState)
runCommandWith inp out err command state = 
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
            state' <- reapJobs All state out
            pure (True, state')
        "history" -> do
            (s, state') <- getHistory (args command) HistoryNormal state
            hPutStr out s
            pure (True, state')
        "declare" -> do
            let (s, state') = declareVar state (args command) 
            if null s then pure (True, state') else do 
                hPutStrLn out s
                pure (True, state')
        _ -> do
            result <- findExecutable (cmd command) 
            state' <- case result of
                Just fullPath -> do
                    (_, _, _, processHandle) <-
                        createProcess
                        (proc (cmd command) (args command))
                            { std_out = UseHandle out,
                            std_err = UseHandle err,
                            std_in = UseHandle inp 
                            }
                    if isBgJob command then do
                        pid <- getPid processHandle
                        let osPid = maybe "unknown" show pid
                            j_id = if Map.null (bgJobs state) then 1 else fst (last (Map.toList (bgJobs state))) + 1
                            procInf = ProcInfo {name = cmd command ++ " " ++ unwords (args command), handle = processHandle, status = Running}
                            state' = state {bgJobs = Map.insert j_id procInf (bgJobs state), nextJobId = j_id + 1, latestJobIds = j_id:latestJobIds state}
                        putStrLn $ "[" ++ show j_id ++ "] " ++ osPid
                        pure state'
                    else do
                        _ <- waitForProcess processHandle
                        pure state
                Nothing -> do
                    hPutStrLn out $ cmd command ++ ": command not found"
                    pure state
            pure (True, state')

data HistoryMode =  HistoryNormal | HistoryRead | HistoryWrite | HistoryAppend
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

data ReapMode = DoneOnly | All deriving (Eq)
reapJobs :: ReapMode -> ShellState -> Handle -> IO ShellState

reapJobs mode state out = do 
    jobList <- mapM (\(id, procInfo) -> do
        exit_code <- getProcessExitCode (handle procInfo)
        case exit_code of
            Nothing -> pure (id, procInfo)
            Just _ -> pure (id, procInfo {status = Done})
        ) (Map.toList (bgJobs state))
    let filtJobList = if mode == DoneOnly then filter (\(_, procInfo) -> status procInfo == Done) jobList else jobList
    let addAnd status = if status == Running then " &" else ""
        droppedIds = map fst (filter (\(id, procInfo) -> status procInfo /= Running) jobList)
        newIdList = filter (`notElem` droppedIds) (latestJobIds state) 
    mapM_ (\(jobNum, procState) -> 
        hPutStrLn out $ "[" ++ show jobNum ++ "]" ++ 
        addPlus jobNum (latestJobIds state) ++ "  " ++ show (status procState) 
        ++ name procState ++ addAnd (status procState) ) filtJobList
    let state' = state {bgJobs = Map.fromList (filter (\(_, procInfo) -> status procInfo == Running ) jobList), latestJobIds = newIdList}
    pure state'

addPlus :: Int -> [Int] -> String
addPlus _ [] = ""
addPlus y [x] = if y == x then "+" else ""
addPlus z (x:y:xs) 
    | z == x = "+"
    | z == y = "-"
    | otherwise = ""

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
