module Main (main) where

import System.IO (hFlush, hPutStrLn, stdout, stderr, withFile, Handle, IOMode (WriteMode, AppendMode), NewlineMode (inputNL), stdin, hReady, hPutStr)

import System.Process (proc, createProcess, std_out, std_err, waitForProcess, CreateProcess (cwd, std_in), StdStream (UseHandle, Inherit, CreatePipe), readProcess, ProcessHandle, getPid, getProcessExitCode)
import Control.Monad.IO.Class
import System.Posix.Terminal
import System.Posix.IO (stdInput, closeFd, stdOutput, createPipe, dupTo)
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
import GHC.IO.Exception (IOException(IOError), ExitCode (ExitSuccess, ExitFailure))
import Data.Char (isAlphaNum)
import System.Posix (forkProcess, getProcessStatus, installHandler, keyboardSignal, Handler (Catch), sigINT, ProcessStatus (Exited))
import System.Exit (exitWith)

import Shell.Types
import Shell.Repl
import Shell.History (initializeHistory)
import Shell.Exec (loop)

main :: IO ()
main = do
    enableRawMode
    let init = ShellState {completions = Map.empty, bgJobs = Map.empty, nextJobId = 1, latestJobIds = [], history = [], historyPosition = 0, unappendedHistoryIndex = 0, declares = Map.empty, lastExitCode = ExitSuccess} 
    printPrompt init
    state <- initializeHistory init
    loop "" OtherKey state






