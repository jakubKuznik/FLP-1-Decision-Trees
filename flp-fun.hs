-- Faculty: BUT FIT 
-- Course: FLP 
-- Project Name: Desicion Trees 
-- Name: Jakub Kuznik
-- Login: xkuzni04
-- Year: 2024


-- execution: 
-- flp-fun -1 <file-with-three> <file-with-new-data>
-- flp-fun -2 <file with training data> 

--libs 
-- base, containers, parsec, vector, split, directory, random. !!
import qualified System.Environment as SE (getArgs)
import System.IO.Error as SYSIOE (userError)
import Control.Exception as CONE (throwIO)
import Data.List as DLIST (sortBy)
import Data.List.Split as DLSPLIT (splitOn)

-- DATA TYPES:
-- Files for classification  

-- a should be Ord 
data DTree = EmptyDTree | Leaf String | Node Int Float (DTree ) (DTree ) 
    deriving (Show, Read, Eq)

-- ERRORS 
myError :: Int -> IO ()
myError 1 = CONE.throwIO $ SYSIOE.userError  "Wrong arguments format."
myError 2 = CONE.throwIO $ SYSIOE.userError  "No arguments given."
myError 3 = CONE.throwIO $ SYSIOE.userError  "Cannot classify data."
myError _ = CONE.throwIO $ SYSIOE.userError  "Unknown Error."

-- TODO delete 
printTree :: DTree -> Int -> IO ()
printTree (Leaf str) indent = putStrLn $ replicate indent ' ' ++ "Leaf: " ++ str
printTree (Node flag bound left right) indent = do
    putStrLn $ replicate indent ' ' ++ "Node: " ++ show flag ++ ", " ++ show bound
    printTree left (indent + 2)
    printTree right (indent + 2)
printTree EmptyDTree _ = putStrLn "<Empty>"

-- TASK 1 --------------------
loadTree :: [String] -> IO ()
loadTree []           = myError 1
loadTree [_]          = myError 1
loadTree [arg1, arg2] = do 
    treeFile <- readFile arg1 
    let treeLines = lines treeFile
    let tree = buildTree treeLines 
    dataFile <- readFile arg2
    let dataLines = lines dataFile
    evaluateData tree dataLines
loadTree (_:_)        = myError 1

getOnIndex :: [Float] -> Int -> Float
getOnIndex (x:_) 0 = x
getOnIndex (_:xs) i = getOnIndex xs (i-1)
getOnIndex _ _ = 0 --todo error

evaluateLine :: DTree -> [Float] -> IO ()
evaluateLine (Node i f l r) xs = do
    let x = getOnIndex xs i 
    if x <= f
        then do 
            evaluateLine l xs 
        else do 
            evaluateLine r xs 
evaluateLine (Leaf s) _ = do
    putStrLn s 
evaluateLine _ _ = myError 3

evaluateData :: DTree -> [String] -> IO ()
evaluateData t (x:xs) = do
    let line = map read . splitOn "," $ removeSpaces x :: [Float]
    evaluateLine t line
    evaluateData t xs
evaluateData _ [] = return () 

countSpaces :: String -> Int
countSpaces [] = 0
countSpaces (x:xs)
    | x == ' ' = 1 + countSpaces xs 
    | otherwise = 0

removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (x:xs)
    | x == ' ' = removeSpaces xs
    | otherwise = x : removeSpaces xs

-- From: Node: 0, 5.5
-- Get: [0, 5.5]
getNodeString :: String -> (String, String)
getNodeString x = let 
    [_, rest] = DLSPLIT.splitOn ":" x
    [a, b] = DLSPLIT.splitOn "," rest
    in (a,b)

-- find lines with specific number of spaces 
findNodeStrings :: [String] -> Int -> [String]
findNodeStrings [] _ = []
findNodeStrings (x:xs) i 
    | countSpaces x == i = x : findNodeStrings xs i 
    | otherwise = findNodeStrings xs i 

-- Find String that represent successor in string stream 
-- findNode [String] "Left" nodeSpaces 
findNodeString :: [String] -> String ->  Int -> String -> String
findNodeString (x:xs) "Left" i "" = do
    let linesFound = findNodeStrings (x:xs) (i)
    case linesFound of
        []      -> ""
        [s]     -> s 
        (s:_:_)   -> s 
findNodeString (x:xs) "Right" i "" = do
    let linesFound = findNodeStrings (x:xs) (i)
    case linesFound of
        []      -> ""
        [_]     -> ""
        (_:s:_) -> s 
findNodeString (x:xs) a i p
    | p == x = findNodeString xs a i ""
    | otherwise = findNodeString xs a i p
findNodeString _ _ _ _ = "" 

-- Successor always has spaces + 2 
buildTree :: [String] -> DTree 
buildTree (x:xs) = buildNode (x:xs) x (countSpaces x)  
buildTree _ = EmptyDTree 

-- Give me string of the node 
-- Int is a spaces of builded node 
buildNode :: [String] -> String -> Int -> DTree
buildNode _ [] _ = EmptyDTree
buildNode (x:xs) r i = 
    let 
        nodeString = removeSpaces r
        (nodeType, rest) = break (== ':') nodeString 
    in 
        case nodeType of
            "Node"  ->
                let (a, b) = getNodeString rest
                    leftTree = buildLeft  (x:xs) (i + 2) r 
                    righTree = buildRight (x:xs) (i + 2) r 
                in Node (read a :: Int) (read b :: Float) leftTree righTree
            "Leaf"  -> 
                let [_,b] = DLSPLIT.splitOn ":" rest
                in Leaf b 
            _ -> EmptyDTree
buildNode _ _ _ = EmptyDTree

-- Int is parentspaces
buildRight :: [String] -> Int -> String -> DTree
buildRight [] _ _ = EmptyDTree
buildRight (x:xs) i p =  
    let r = findNodeString (x:xs) "Right" i p 
    in buildNode (x:xs) r i

-- Int is parentspaces 
-- String is parrent string 
buildLeft :: [String] -> Int -> String -> DTree
buildLeft [] _ _ = EmptyDTree
buildLeft (x:xs) i p = 
    let r = findNodeString (x:xs) "Left" i p
    in buildNode (x:xs) r i 

------------------------------

-- TASK2 ---------------------
-- find potential split (where the classes differ)
-- 1 TA  --> split point 1.5 
-- 2 TB  
-- ([row,line of TA],Float GIMI)
-- find lowest GIMI 
-- remove row 
-- build node 
-- split into two list (LN, RN)
trainTree :: [String] -> IO ()
trainTree []     = myError 1
trainTree [arg1] = do
    dataFile <- readFile arg1 
    let dataLines = lines dataFile 
    let parsedData = sorteList 0 (parseFile dataLines)
    let potentialSplits = fPoSpInCo (0,0) (0.0,"",0) parsedData
    putStrLn $ show potentialSplits
    putStrLn $ show parsedData
trainTree (_:_)  = myError 1
    
-- let sortedData = sortedList 0 parsedData  

-- (row,column) -> (prev_float, previous_Class,b) -> Data -> (row,column)
-- findPotentialSplitsInColumn 
fPoSpInCo :: (Int, Int) -> (Float, String, Int) -> [([Float], String)] -> [(Int, Int)]
fPoSpInCo (0,c) (_,"",b) (((f:fr),s):r) 
    | c /= b    = fPoSpInCo (0,c) (0.0,s,b+1) ((fr,s) : r)
    | otherwise = fPoSpInCo (1,c) (f,s,0) r
fPoSpInCo (n,c) (pf,pc,b) (((f:fr),s):r)
    | c /= b    = fPoSpInCo (n,c) (pf,pc,b+1) ((fr,s) : r)
    | pc == s   = fPoSpInCo (n+1, c) (f, s, 0) r
    | otherwise = (n,c) : fPoSpInCo (n+1, c) (f, s, 0) r
fPoSpInCo _ _ _ = []

compareNthFloat :: Int -> ([Float], String) -> ([Float], string) -> Ordering
compareNthFloat n (a, _) (b, _) = compare (a !! n) (b !! n)

sorteList :: Int -> [([Float], String)] -> [([Float], String)]
sorteList n k = sortBy (compareNthFloat n) k 

parseFile :: [String] -> [([Float], String)]
parseFile [] = []
parseFile (x:xs) = let
    a = DLSPLIT.splitOn "," x
    in (map read (init a) :: [Float], last a) : parseFile xs 


-------------------------------

-- association list. for command line argument
-- source: Learn You a Haskell for a Great Good!
dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("-1", loadTree)
            , ("-2", trainTree)
            ]

main :: IO ()
main = do
    args <- SE.getArgs
    case args of  
        [] -> myError 2 -- no arguments given
        (command:restArgs) -> do
            let action = lookup command dispatch 
            case action of
                Just a -> a restArgs 
                Nothing -> myError 1 