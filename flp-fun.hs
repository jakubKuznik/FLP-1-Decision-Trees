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


--data Node 

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
    buildTree fileLines
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

-- Successor always has spaces + 2 
buildTree :: [String] -> IO ()
buildTree (x:xs) = do
    let space = countSpaces x
    putStr (show space)
    putStr " "
    putStrLn $ removeSpaces x
    let lineNode = removeSpaces x
    let (nodeType, rest) = break (== ':') lineNode 
    case nodeType of
        "Node" -> do
            putStr "Node"
            let [_, rest'] = DLSPLIT.splitOn ":" rest
            let [a, b] = DLSPLIT.splitOn "," rest' 
            let leftTree = buildLeft xs lineNode
            let righTree = buildRight xs lineNode
            putStr a
            putStr b
            let newNode = Node (read a :: Int) (read b :: Float) leftTree righTree
            putStrLn $ show newNode
        "Leaf" -> do
            putStr "Leaf"
            let [_,b] = DLSPLIT.splitOn ":" rest
            let newLeaf = Leaf b
            putStrLn $ show newLeaf
        _ -> do
            myError 3
    buildTree xs
buildTree _ = putStrLn "end"

buildRight :: (Ord parrentFlor) => [String] -> parrentFlor -> DTree
buildRight _ _ = EmptyDTree 

buildLeft :: (Ord parrentFlor) => [String] -> parrentFlor -> DTree
buildLeft _ _ = EmptyDTree 

-- buildTree _ = Node 0 5.5 EmptyDTree EmptyDTree
 
 
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