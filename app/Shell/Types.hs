module Shell.Types where

import qualified Data.Map as Map
import System.Process (ProcessHandle)
import GHC.IO.Exception (IOException(IOError), ExitCode (ExitSuccess, ExitFailure))
    
data Command = Command {cmd :: String, args :: [String], redirect :: Maybe (Redirect, String), isBgJob :: Bool}

data Redirect = OutWrite | OutAppend | ErrWrite| ErrAppend

data ParseMode = Redir Redirect | Norm | Bg

data CompleteMode = Awaiting | Print | AddPath | AddName String | Remove

data HistoryMode =  HistoryNormal | HistoryRead | HistoryWrite | HistoryAppend

data ReapMode = DoneOnly | All deriving (Eq)

data ProcStatus = Running | Done deriving (Eq)
instance Show ProcStatus where
    show Running = "Running" ++ replicate 17 ' '
    show Done = "Done" ++ replicate 20 ' '

data ProcInfo = ProcInfo {handle :: ProcessHandle, 
                        name :: String, 
                        status :: ProcStatus }
data ShellState = ShellState {completions :: Map.Map String FilePath, 
                            bgJobs :: Map.Map Int ProcInfo, 
                            nextJobId :: Int, 
                            latestJobIds :: [Int], 
                            history :: [String], 
                            historyPosition :: Int, 
                            unappendedHistoryIndex :: Int, 
                            declares :: Map.Map String String, 
                            lastExitCode :: ExitCode
                            }

data KeyType = TabKey | OtherKey

data TokenState = Normal Bool | SingleQuote | DoubleQuote | Backslash TokenState

data SubMode = SubNorm | SubDollar | SubOpenBrace