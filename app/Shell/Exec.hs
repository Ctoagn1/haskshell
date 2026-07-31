module Shell.Exec where

import Shell.Types
import GHC.IO.Handle (Handle, hPutStr, hFlush)
import GHC.IO.Exception (ExitCode (ExitSuccess, ExitFailure))
import System.Posix (ProcessStatus(Exited), stdInput, stdOutput, forkProcess, dupTo, closeFd, getProcessStatus, createPipe)
import System.Process (ProcessHandle, proc, CreateProcess (std_out, std_err, std_in), waitForProcess, createProcess, StdStream (UseHandle), getPid)
import Control.Monad (replicateM, forM, when, void)
import System.Exit (exitWith)
import GHC.IO.Handle.FD (stdin, stdout, stderr, withFile)
import GHC.IO.IOMode (IOMode(WriteMode, AppendMode))
import Shell.Parsing (resolvePath, isBuiltin, parsePipeline, tokenize)
import System.Directory (getCurrentDirectory, doesDirectoryExist, setCurrentDirectory, findExecutable)
import GHC.IO.Handle.Text (hPutStrLn)
import Shell.Jobs (reapJobs)
import qualified Data.Map as Map
import System.IO (hReady)
import Shell.Completions (completeFunc, handleCompletion)
import Shell.History (getHistory, saveHistory)
import Shell.Variables (declareVar, substituteVars)
import Shell.Repl (printPrompt, handleDownArrow, handleUpArrow)

executePipeline :: [Command] -> ShellState -> IO ExitCode
executePipeline commands s = do
    pipes <- replicateM (length commands - 1 ) createPipe
    let inFds = stdInput : map fst pipes
        outFds = map snd pipes ++ [stdOutput]
    pids <- forM (zip3 commands inFds outFds) $ \(c, inFd, outFd) ->
        forkProcess (do
            when (inFd /= stdInput) (void (dupTo inFd stdInput))
            when (outFd /= stdOutput) (void (dupTo outFd stdOutput))
            mapM_ (\(r, w) -> closeFd r >> closeFd w) pipes
            (_, _, h) <- runCommand c s
            z <- case h of
                    Right x -> pure x
                    Left x -> waitForProcess x
            exitWith z
            )

    mapM_ (\(r, w) -> closeFd r >> closeFd w) pipes
    stats <- mapM (getProcessStatus True False) pids
    case last stats of
        Just status -> do 
            case status of
                Exited c -> pure c
                _  -> pure ExitSuccess



runCommand :: Command -> ShellState -> IO (Bool, ShellState, Either ProcessHandle ExitCode)
runCommand command state =
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
                
runCommandWith :: Handle -> Handle -> Handle -> Command -> ShellState -> IO (Bool, ShellState, Either ProcessHandle ExitCode)
runCommandWith inp out err command state = 
    case cmd command of
        "exit" ->
            pure (False, state, Right ExitSuccess)
        "pwd" -> do
            cwd <- getCurrentDirectory
            hPutStrLn out cwd
            pure (True, state, Right ExitSuccess)
        "cd" -> do
            let arg = unwords (args command)
            newCwd <- resolvePath arg
            isDir <- doesDirectoryExist newCwd
            if isDir then do
                setCurrentDirectory newCwd
                pure (True, state, Right ExitSuccess)
            else do
                hPutStrLn err ("cd: " ++ arg ++ ": No such file or directory")
                pure (True, state, Right (ExitFailure 1))

        "echo" -> do
            hPutStrLn out (unwords (args command))
            pure (True, state, Right ExitSuccess)
        "type" -> do
            let arg = unwords (args command)
            if isBuiltin arg
                then do 
                    hPutStrLn out $ arg ++ " is a shell builtin"
                    pure (True, state, Right ExitSuccess)
                else do
                result <- findExecutable arg
                case result of
                    Just fullPath -> do
                        hPutStrLn out $ arg ++ " is " ++ fullPath
                        pure (True, state, Right ExitSuccess)
                    Nothing -> do
                        hPutStrLn err $ arg ++ ": not found"
                        pure (True, state, Right (ExitFailure 1))
        "complete" -> do
            (str, nstate) <- completeFunc (args command) Awaiting state
            if null str then pure 
                (True, nstate, Right ExitSuccess)
            else do
                hPutStrLn out str
                pure (True, nstate, Right ExitSuccess)
        "jobs" -> do
            state' <- reapJobs All state out
            pure (True, state', Right ExitSuccess)
        "history" -> do
            (s, state') <- getHistory (args command) HistoryNormal state
            hPutStr out s
            pure (True, state', Right ExitSuccess)
        "declare" -> do
            let (s, state') = declareVar state (args command) 
            if null s then pure (True, state', Right ExitSuccess) else do 
                hPutStrLn out s
                pure (True, state', Right ExitSuccess)
        _ -> do
            result <- findExecutable (cmd command) 
            case result of
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
                        pure (True, state', Right ExitSuccess)
                    else do
                        pure (True, state, Left processHandle)
                Nothing -> do
                    hPutStrLn out $ cmd command ++ ": command not found"
                    pure (True, state, Right (ExitFailure 1))


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
                let run x st = case x of
                            [] -> pure (True, st, Right (lastExitCode st) )
                            [y] -> runCommand y st 
                            _ -> do 
                                c <- executePipeline x st
                                pure (True, st, Right c)
                                 
                (continue, nstate, h) <- run (parsePipeline $ substituteVars (tokenize buf) s) s
                exCode <- case h of
                    Right y -> pure y
                    Left x -> waitForProcess x
                let updatedState = nstate {lastExitCode = exCode}
                if continue then do
                    state' <- reapJobs DoneOnly updatedState stdout 
                    printPrompt state'
                    loop "" OtherKey state'
                else do
                    saveHistory nstate
                    pure ()
        '\t' -> do 
            (a, b, c) <- handleCompletion buf prev state
            loop a b c
            
        '\DEL' -> if null buf then loop buf OtherKey state else do
            putStr "\b  \b\b"
            hFlush stdout
            loop (init buf) OtherKey state 
        '\ESC' -> do
            is_an_arrow <- hReady stdin
            when is_an_arrow $ do
                c2 <- getChar
                c3 <- getChar
                (a, b, c) <- case (c2, c3) of
                    ('[', 'A' ) -> handleUpArrow buf state
                    ('[', 'B') -> handleDownArrow buf state
                    _ -> pure (buf, OtherKey, state)
                loop a b c
        _ -> do
            putChar ch
            hFlush stdout
            loop (buf ++ [ch]) OtherKey state
