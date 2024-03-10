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



foo :: String -> String -> String 
foo a _ = a 

main :: IO ()
main = do
    (command:args) <- SE.getArgs
    case args of
        (arg:_) -> putStrLn $ foo command arg
        _       -> putStrLn "No arguments provided."