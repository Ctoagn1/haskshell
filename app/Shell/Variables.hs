module Shell.Variables where
import Shell.Types
import qualified Data.Map as Map
import Shell.Repl
import Data.Char (isAlphaNum, isAlpha)
import Shell.Parsing (splitOn)

substituteVars :: [(String, Bool)] -> ShellState -> [(String, Bool)]
substituteVars inp state = filter (not . null) (map (\x ->if (not . snd) x then (varSub (fst x) "" "" state SubNorm, False) else x) inp)

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
