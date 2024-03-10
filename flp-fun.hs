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
import Data.List.Split as DLSPLIT (splitOn)

-- Implement parth that load decisssion-tree 
-- in specific format 
--Node: 0, 5.5 
--  Leaf: TridaA
--  Node: 1, 3.0
--    Leaf: TridaB
--    Leaf: TridaC

-- Implement a part, that for loadaded tree and new data 
-- decide result class. 
-- New data syntax: 
-- 2.4, 1.3
-- 6.1, 0.3
-- 6.3, 4.4

--Example output for the tree that is here:
--TridaA
--TridaB
--TridaC

-- DATA TYPES:
-- Files for classification  

-- a should be Ord 
data DTree = EmptyDTree | Leaf String | Node Int Float (DTree ) (DTree ) 
    deriving (Show, Read, Eq)

smallTree :: DTree 
smallTree =
    Node 0 5.5
        (Leaf "TridaA")
        (Node 1 3.0
            (Leaf "TridaB")
            (Leaf "TridaC")
        )

-- ERRORS 
myError :: Int -> IO ()
myError 1 = CONE.throwIO $ SYSIOE.userError  "Wrong arguments format."
myError 2 = CONE.throwIO $ SYSIOE.userError  "No arguments given."
myError _ = CONE.throwIO $ SYSIOE.userError  "Unknown Error."

-- TASK 1 --------------------
loadTree :: [String] -> IO ()
loadTree []           = myError 1
loadTree [_]          = myError 1
loadTree [arg1, arg2] = do 
    content <- readFile arg1 
    let fileLines = lines content
    -- let tree = buildTree fileLines
    let tree = buildTree fileLines
    putStrLn $ show tree
    putStrLn $ show smallTree
    --putStrLn $ show tree
loadTree (_:_)        = myError 1

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

-- From: 
--  Node: 0, 5.5
-- Get:
-- [0, 5.5]
getNodeString :: String -> (String, String)
getNodeString x = let 
    [_, rest] = DLSPLIT.splitOn ":" x
    [a, b] = DLSPLIT.splitOn "," rest
    in (a,b)

-- find lines with specific spaces 
findNodeStrings :: [String] -> Int -> [String]
findNodeStrings [] _ = []
findNodeStrings (x:xs) i 
    | countSpaces x == i = x : findNodeStrings xs i 
    | otherwise = findNodeStrings xs i 

-- Find String that represent successor in string stream 
-- findNode [String] "Left" ParentSpaces
findNodeString :: [String] -> String ->  Int -> String
findNodeString (x:xs) "Left" i = do
    let linesFound = findNodeStrings (x:xs) (i)
    case linesFound of
        []      -> ""
        [s]     -> s 
        [s,_]   -> s 
        _       -> ""
findNodeString (x:xs) "Right" i = do
    let linesFound = findNodeStrings (x:xs) (i)
    case linesFound of
        []      -> ""
        [_]     -> ""
        [_,s]   -> s 
        _       -> ""
findNodeString _ _ _ = "" 

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
                -- TODO remove the r from the x:xs 
                let (a, b) = getNodeString rest
                    leftTree = buildLeft  (x:xs) (i + 2)  
                    righTree = buildRight (x:xs) (i + 2) 
                in Node (read a :: Int) (read b :: Float) leftTree righTree
            "Leaf"  -> 
                let [_,b] = DLSPLIT.splitOn ":" rest
                in Leaf b 
            _ -> EmptyDTree
buildNode _ _ _ = EmptyDTree

-- Successor always has spaces + 2 
buildTree :: [String] -> DTree 
buildTree (x:xs) = buildNode (x:xs) x (countSpaces x)  
buildTree _ = EmptyDTree 

-- Int is parrentspaces
buildRight :: [String] -> Int -> DTree
buildRight [] _ = EmptyDTree
buildRight (x:xs) i =  
    let r = findNodeString (x:xs) "Right" i
    in buildNode (x:xs) r i

-- Int is parrentspaces
buildLeft :: [String] -> Int -> DTree
buildLeft [] _ = EmptyDTree
buildLeft (x:xs) i = 
    let r = findNodeString (x:xs) "Left" i
    in buildNode (x:xs) r i 

------------------------------

-- TASK2 ---------------------
trainTree :: [String] -> IO ()
trainTree []     = myError 1
trainTree [arg1] = putStrLn arg1
trainTree (_:_)  = myError 1 
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