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
data DTree = EmptyDTree | Leaf String | Node Int Float (DTree ) (DTree ) 
    deriving (Show, Read, Eq)
type TFile      = [TFLine]
type TFLine     = ([Float],String)
type TColumn    = [(Float,String)]
type GiniNums   = [(Int,Int,Float)]

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







-- data DTree = EmptyDTree | Leaf String | Node Int Float (DTree ) (DTree ) 
--     deriving (Show, Read, Eq)

-- TASK2 ---------------------
trainTree :: [String] -> IO ()
trainTree []     = myError 1
trainTree [arg1] = do
    dataFile <- readFile arg1 
    let dataLines   = lines dataFile 
    let parsedData  = parseFile dataLines

    putStrLn $ show $ trainTreeBuild parsedData    

    let c           = getClass parsedData
    let classes     = removeRedundantClass c
    let cards       = buildCARD parsedData classes 0 
    let initMax     = 1.0::Float
    let best        = foldl (\a (_, _, e) -> if e < a then e else a) initMax cards
    let position    = case head $ filter (\(_, _, e) -> e == best) cards of
                    (x, y, _) -> (x, y)
    let sortedData  = sorteList (fst position) parsedData
    let (l1,l2)     = splitAt (snd position) sortedData
    putStrLn "Cards "
    putStrLn $ show cards
    putStrLn "Best"
    putStrLn $ show  best
    putStrLn "Positions"
    putStrLn $ show position 
    putStrLn $ show $ getNthColumn (sorteList 0 parsedData ) 0
    putStrLn $ show $ getNthColumn (sorteList 1 parsedData ) 1 
    putStrLn $ show $ sortedData 
    putStrLn $ show $ l1 
    putStrLn $ show $ l2 


trainTree (_:_)  = myError 1

-- type TFile      = [TFLine]
-- type TFLine     = ([Float],String)
trainTreeBuild :: TFile -> DTree
trainTreeBuild [] = EmptyDTree
trainTreeBuild f = 
    let
        c            = getClass f 
        classes      = removeRedundantClass c
        cards        = buildCARD f classes 0 
        initMax      = 1.0::Float
        best         = foldl (\a (_, _, e) -> if e < a then e else a) initMax cards
        bestPosition = case head $ filter (\(_, _, e) -> e == best) cards of
                        (x, y, _) -> (x, y)
         
    in EmptyDTree

getClass :: TFile -> [String]
getClass ((_,s):r) = s : getClass r
getClass [] = []

removeRedundantClass :: Eq a => [a] -> [a]
removeRedundantClass (x:xs) = x : removeRedundantClass (filter (/= x) xs)
removeRedundantClass [] = []

getNthColumn :: TFile -> Int -> TColumn 
getNthColumn dataList n = [(floatList !! n, str) | (floatList, str) <- dataList]

-- from data build tree using CARD method
-- Int <==> column 
buildCARD :: TFile -> [String] -> Int -> GiniNums 
buildCARD da t c 
    | c >= (length (fst ( head da))) = []
    | otherwise =
        let sortedData      = sorteList c da 
            oneColumnData   = getNthColumn sortedData c
            potentialSplits = filter (\x -> x < (length da)) (fPoSpInCo (c, 1) (0.0, "", 0) sortedData)
            gini            = countGINI oneColumnData potentialSplits t
            colGini         = map (\x -> (c, fst x, snd x)) gini
        in colGini ++ buildCARD da t (c+1) 

-- Potential splits rows, all clasess, column, (Row, GINI)
-- (Int,Int) --> (Desired, Current) 
countGINI :: [(Float, String)] -> [Int] -> [String] -> [(Int,Float)]
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

-- let flUpper     = countGiniSmall 0 (length l1) classes1 
countGiniSmall :: Float -> Int -> [Int] -> Float
countGiniSmall f _ []     = f
countGiniSmall f s (x:xs) = 
    countGiniSmall (f + (fromIntegral (x*x) / fromIntegral (s*s))) s xs

-- let sortedData = sortedList 0 parsedData  
-- (row,column) -> (prev_float, previous_Class,b) -> Data -> (row,column)
-- findPotentialSplitsInColumn
-- for a given column returns a list of (rows,columns) where there is potencial split
-- CARD method 
fPoSpInCo :: (Int, Int) -> (Float, String, Int) -> TFile -> [Int]
fPoSpInCo (0,c) (_,_,b) (([f],s):r)   -- if i am on the last column
    | c > b      = [-1]
    | otherwise  = fPoSpInCo (1,c) (f,s,0) r
fPoSpInCo (0,c) (_,"",b) (((f:fr),s):r) -- get to given column  
    | c /= b    = fPoSpInCo (0,c) (0.0,s,b+1) ((fr,s) : r)
    | otherwise = fPoSpInCo (1,c) (f,s,0) r
fPoSpInCo (n,c) (pf,pc,b) (((f:fr),s):r) -- check if class is differ 
    | c /= b    = fPoSpInCo (n,c) (pf,pc,b+1) ((fr,s) : r)
    | pc == s   = fPoSpInCo (n+1, c) (f, s, 0) r
    | otherwise = (n) : fPoSpInCo (n+1, c) (f, s, 0) r
fPoSpInCo _ _ _ = []

compareNthFloat :: Int -> TFLine -> TFLine -> Ordering
compareNthFloat n (a, _) (b, _) = compare (a !! n) (b !! n)

sorteList :: Int -> TFile -> TFile 
sorteList n k = sortBy (compareNthFloat n) k 

parseFile :: [String] -> TFile 
parseFile [] = []
parseFile (x:xs) = let
    a = DLSPLIT.splitOn "," x
    in (map read (init a) :: [Float], last a) : parseFile xs 
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