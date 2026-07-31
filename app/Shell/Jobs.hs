module Shell.Jobs where 
import Shell.Types
import GHC.IO.Handle (Handle)
import System.Process (getProcessExitCode)
import qualified Data.Map as Map
import GHC.IO.Handle.Text (hPutStrLn)

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

