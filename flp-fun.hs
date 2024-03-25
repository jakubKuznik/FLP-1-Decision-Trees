-- Faculty: BUT FIT 
-- Course: FLP 
-- Project Name: Desicion Trees 
-- Name: Jakub Kuznik
-- Login: xkuzni04
-- Year: 2024

-- execution: 
-- flp-fun -1 <file-with-three> <file-with-new-data>
-- flp-fun -2 <file with training data> 

import qualified System.Environment as SE (getArgs)
import System.IO.Error as SYSIOE (userError)
import Control.Exception as CONE (throwIO)
import Data.List as DLIST (sortBy,nub)
import Data.List.Split as DLSPLIT (splitOn)

-- DATA TYPES:
data DTree = EmptyDTree | Leaf String | Node Int Double (DTree ) (DTree ) 
    deriving (Show, Read, Eq)
type TFile      = [TFLine]
type TFLine     = ([Double],String)
type TColumn    = [(Double,String)]
type GiniNums   = [(Int,Int,Double)]
type GiniRow    = [(Int,Double)]

myError :: Int -> IO ()
myError 1 = CONE.throwIO $ SYSIOE.userError  "Wrong arguments format."
myError 2 = CONE.throwIO $ SYSIOE.userError  "No arguments given."
myError 3 = CONE.throwIO $ SYSIOE.userError  "Cannot classify data."
myError _ = CONE.throwIO $ SYSIOE.userError  "Unknown Error."

-- TASK 1 --------------------
loadTree :: [String] -> IO ()
loadTree []           = myError 1
loadTree [arg1, arg2] = do 
    treeFile <- readFile arg1 
    let treeLines = filter (not. null) $ lines treeFile
    let tree = buildTree treeLines 
    dataFile <- readFile arg2
    let dataLines = filter (not. null) $ lines dataFile
    evaluateData tree dataLines
loadTree (_:_)        = myError 1

getOnIndex :: [Double] -> Int -> Double 
getOnIndex (x:_) 0 = x
getOnIndex (_:xs) i = getOnIndex xs (i-1)
getOnIndex _ _ = 0 

evaluateLine :: DTree -> [Double] -> IO ()
evaluateLine (Node i f l r) xs = do
    let x :: Double 
        x = getOnIndex xs i 
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
    let line = map read $ splitOn "," $ removeSpaces x :: [Double]
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

-- find lines with specific number of spaces and its line 
findNodeStrings :: [String] -> Int -> Int -> [(String,Int)]
findNodeStrings [] _ _ = []
findNodeStrings (x:xs) i c 
    | countSpaces x == i = (x,c) : findNodeStrings xs i (c+1)
    | otherwise = findNodeStrings xs i (c+1) 

-- Find String that represent successor in string stream 
-- findNode [String] "Left" nodeSpaces 
findNodeString :: [String] -> String ->  Int -> String -> (String, Int)
findNodeString (x:xs) "Left" i ""
    | not (null linesFound) = head linesFound
    | otherwise = ("", 0)
    where linesFound = findNodeStrings (x:xs) i 0
findNodeString (x:xs) "Right" i ""
    | length linesFound > 1 = linesFound !! 1
    | otherwise = ("", 0)
    where linesFound = findNodeStrings (x:xs) i 0
findNodeString (x:xs) a i p
    | p == x = findNodeString xs a i ""
    | otherwise = findNodeString xs a i p
findNodeString _ _ _ _ = ("", 0)

-- Successor always has spaces + 2 
buildTree :: [String] -> DTree 
buildTree (x:xs) = buildNode (x:xs) x (countSpaces x)  
buildTree _ = EmptyDTree 

-- Give me string of the node 
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
                in Node (read a :: Int) (read b :: Double) leftTree righTree
            "Leaf"  -> 
                let [_,b] = DLSPLIT.splitOn ":" rest
                in Leaf b 
            _ -> EmptyDTree
buildNode _ _ _ = EmptyDTree

-- String is parrent string 
buildRight :: [String] -> Int -> String -> DTree
buildRight [] _ _ = EmptyDTree
buildRight (x:xs) i p =  
    let (r,s) = findNodeString (x:xs) "Right" i p 
    in buildNode (drop s(x:xs)) r i

buildLeft :: [String] -> Int -> String -> DTree
buildLeft [] _ _ = EmptyDTree
buildLeft (x:xs) i p = 
    let (r,s) = findNodeString (x:xs) "Left" i p
    in buildNode (drop s (x:xs)) r i 

-- TASK2 ---------------------
trainTree :: [String] -> IO ()
trainTree []     = myError 1
trainTree [arg1] = do
    dataFile <- readFile arg1 
    let dataLines   = filter (not . null) $ lines dataFile 
    let parsedData  = parseFile dataLines
    printTree (trainTreeBuild parsedData) 2
trainTree (_:_)  = myError 1

trainTreeBuild :: TFile -> DTree
trainTreeBuild [] = EmptyDTree
trainTreeBuild f = 
    let
        c            = getClass f 
        classes      = removeRedundantClass c
        uniqueClass = length $ DLIST.nub $ map snd f 
    in 
        if uniqueClass <= 1 then
            Leaf (snd $ head f)
        else
            let
                sortedData   = sorteList (fst bestPosition) f 
                cards        = buildCARD f classes 0 
                initMax      = 1.0::Double
                best         = foldl (\a (_, _, e) -> if e < a then e else a) initMax cards
                bestPosition = case head $ filter (\(_, _, e) -> e == best) cards of
                                (x, y, _) -> (x, y)
                (l1,l2)      = splitAt (snd bestPosition) sortedData
                doubleMiddle = ((fst $ last $ getNthColumn l1 (fst bestPosition)) 
                            + (fst $ head $ getNthColumn l2 (fst bestPosition))) / 2 
            in
                Node (fst bestPosition) doubleMiddle (trainTreeBuild l1) (trainTreeBuild l2)

getClass :: TFile -> [String]
getClass ((_,s):r) = s : getClass r
getClass [] = []

removeRedundantClass :: Eq a => [a] -> [a]
removeRedundantClass (x:xs) = x : removeRedundantClass (filter (/= x) xs)
removeRedundantClass [] = []

getNthColumn :: TFile -> Int -> TColumn 
getNthColumn dataList n = [(doubleList !! n, str) | (doubleList, str) <- dataList]

-- from data build tree using CARD method
-- Int <==> column 
buildCARD :: TFile -> [String]  -> Int -> GiniNums 
buildCARD da t c 
    | c >= (length (fst ( head da))) = []
    | otherwise =
        let sortedData      = sorteList c da 
            oneColumnData   = getNthColumn sortedData c
            potentialSplits = filter (\x -> x < (length da)) (findSplits 0 "" oneColumnData)
            gini            = countGINI oneColumnData potentialSplits t
            colGini         = map (\x -> (c, fst x, snd x)) gini
        in colGini ++ buildCARD da t (c+1) 

-- Potential splits rows, all clasess, column, (Row, GINI)
-- (Int,Int) --> (Desired, Current) 
countGINI :: TColumn -> [Int] -> [String] -> GiniRow 
countGINI _ [] _ = [] 
countGINI da (x:xs) t = 
    let (l1,l2)     = splitAt x da
        totalSize   = fromIntegral (length l1 + length l2)
        ratio1      = fromIntegral (length l1) / totalSize
        ratio2      = fromIntegral (length l2) / totalSize
        classes1    = countMatches t (map snd l1)
        classes2    = countMatches t (map snd l2)
        flUpper     = 1.0 - (countGiniSmall 0 (length l1) (map snd classes1))
        flDown      = 1.0 - (countGiniSmall 0 (length l2) (map snd classes2))
        gini        = (ratio1 * flUpper) + (ratio2 * flDown)
    in (x,gini) : countGINI da xs t

countMatches :: [String] -> [String] -> [(String,Int)]
countMatches [] _ = []  
countMatches (x:xs) a = (x, (length (filter (== x) a))) : countMatches xs a 

countGiniSmall :: Double -> Int -> [Int] -> Double 
countGiniSmall f _ []     = f
countGiniSmall f s (x:xs) = 
    countGiniSmall (f + (fromIntegral (x*x) / fromIntegral (s*s))) s xs

findSplits :: Int -> String -> TColumn -> [Int]
findSplits 0 _ ((_,s):rs) = findSplits 1 s rs 
findSplits n pc ((_,s):rs) -- check if class is differ 
    | pc == s   = findSplits (n+1) s rs
    | otherwise = (n) : findSplits (n+1) s rs
findSplits _ _ _ = []

compareNthDouble :: Int -> TFLine -> TFLine -> Ordering
compareNthDouble n (a, _) (b, _) = compare (a !! n) (b !! n)

sorteList :: Int -> TFile -> TFile 
sorteList n k = sortBy (compareNthDouble n) k 

parseFile :: [String] -> TFile 
parseFile [] = []
parseFile (x:xs) = let
    a = DLSPLIT.splitOn "," x
    in (map read (init a) :: [Double], last a) : parseFile xs 

printTree :: DTree -> Int -> IO ()
printTree (Leaf str) indent = putStrLn $ replicate indent ' ' ++ "Leaf: " ++ str
printTree (Node flag bound left right) indent = do
    putStrLn $ replicate indent ' ' ++ "Node: " ++ show flag ++ ", "  
        ++ show bound 
    printTree left (indent + 2)
    printTree right (indent + 2)
printTree EmptyDTree _ = putStrLn "<Empty>"
-------------------------------

-- association list. for command line argument
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