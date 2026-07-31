

module Shell.Completions 
            (handleCompletion
            , getCompletions
            , getCompletionOutput
            , getCompletedFiles
            , completeFunc
            , commonPrefix
            , longestCommonPrefix
            , getExecutablesFromPATH) where 

import Shell.Types
import Shell.Repl
import Data.List (nub, intercalate, sort, isPrefixOf)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import System.Environment (setEnv, lookupEnv)
import System.FilePath (splitFileName, splitSearchPath, (</>))
import qualified Data.Map as Map
import System.Process (readProcess)
import Shell.Parsing (resolvePath, isDir, builtinNames, executablesInDir)
import System.Directory (doesDirectoryExist, getDirectoryContents)

handleCompletion :: String -> KeyType -> ShellState -> IO (String, KeyType, ShellState)
handleCompletion input prev state = do
    let wds=  words input
    let allwords = if not (null input) && last input == ' ' then wds ++ [""] else wds
    case allwords of
        [] -> pure (input, OtherKey, state)
        [command] -> do
            executables <- getExecutablesFromPATH
            let names = nub (builtinNames ++ executables)
            let matches = filter (input `isPrefixOf`) names
            case (matches, prev) of
                ([], _) -> do
                    putChar '\x07'
                    hFlush stdout
                    pure (input, OtherKey, state)
                ([one], _) -> do
                    replaceToken command (one ++ " ")
                    pure  (one ++ " ", OtherKey, state)
                (_, OtherKey) -> do
                    putChar '\x07'
                    hFlush stdout
                    let complete = longestCommonPrefix matches
                    replaceToken command (longestCommonPrefix matches)
                    pure (complete, TabKey, state)
                (_, TabKey) -> do
                    putChar '\n'
                    putStr $ intercalate "\t" (sort matches)
                    putChar '\n'
                    printPrompt state
                    pure (input, TabKey, state)
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
                    pure (input, OtherKey, state)
                ([one], _) -> do
                    let current_length = length fileName
                        to_put = drop current_length one
                    let ch = if last one == '/' then "" else " "
                    putStr $ to_put ++ ch
                    hFlush stdout
                    pure (input ++ to_put ++ ch, OtherKey, state)
                (_, OtherKey) -> do
                    putChar '\x07'
                    hFlush stdout
                    let current_length = length wd
                        complete = longestCommonPrefix matches
                        to_put = drop current_length complete
                    putStr to_put
                    hFlush stdout
                    pure (input ++ to_put, TabKey, state)
                (_, TabKey) -> do
                    putChar '\n'
                    putStr $ intercalate "\t" (sort matches)
                    putChar '\n'
                    putStr $ "\x1B[38;2;114;54;186mλ:\x1B[0m " ++ input
                    hFlush stdout
                    pure (input, TabKey, state)

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
