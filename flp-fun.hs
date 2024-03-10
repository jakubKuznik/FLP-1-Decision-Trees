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
data DTree a = EmptyDTree | Node a (DTree a) (DTree a) 
    deriving (Show, Read, Eq)

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
    buildTree fileLines
    putStr ""
    -- q fileLines
loadTree (_:_)        = myError 1

buildTree :: [String] -> IO ()
buildTree (x:xs) = do
    putStrLn x
    buildTree xs
buildTree _ = putStr ""
 
 
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