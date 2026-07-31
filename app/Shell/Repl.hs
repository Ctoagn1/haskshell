module Shell.Repl where
import Shell.Types
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))
import System.Posix (TerminalAttributes, getTerminalAttributes, stdInput, withoutMode, TerminalMode (EnableEcho, ProcessInput, StartStopOutput), TerminalState (Immediately), setTerminalAttributes)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Shell.Jobs (reapJobs)

printPrompt :: ShellState -> IO ()
printPrompt state = do
    case lastExitCode state of
        ExitSuccess -> putStr "\x1B[38;2;114;54;186mλ:\x1B[0m "
        ExitFailure _ -> putStr "\x1B[38;2;165;29;45mλ:\x1B[0m "
    hFlush stdout


enableRawMode :: IO TerminalAttributes
enableRawMode = do
    old <- getTerminalAttributes stdInput
    let raw = foldl withoutMode old [EnableEcho, ProcessInput, StartStopOutput]
    setTerminalAttributes stdInput raw Immediately
    pure old

replaceToken :: String -> String -> IO ()
replaceToken old new = do
    putStr (replicate (length old) '\b')
    putStr (replicate (length old) ' ')
    putStr (replicate (length old) '\b')
    putStr new
    hFlush stdout

handleUpArrow :: String -> ShellState -> IO (String, KeyType, ShellState)
handleUpArrow buf state = do
    let newpos = max 0 (historyPosition state - 1)
        s = state {historyPosition = newpos}
    if newpos < length (history state) then do
        let new = history state !! newpos
        replaceToken buf new
        pure (new, OtherKey, s)
    else do
        replaceToken buf ""
        pure ("", OtherKey, s)

handleDownArrow :: String -> ShellState -> IO (String, KeyType, ShellState)
handleDownArrow buf state = do
    let newpos = min (length (history state)) (historyPosition state + 1)
        s = state {historyPosition = newpos}
    if newpos < length (history state) then do
        let new = history state !! newpos
        replaceToken buf new
        pure (new, OtherKey, s)
    else do
        replaceToken buf ""
        pure ("", OtherKey, s)
    