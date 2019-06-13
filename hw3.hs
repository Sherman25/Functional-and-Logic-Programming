
-- **********************************************************
-- Problem 1 - Implementation of the AES encryption algorithm

import Data.Char
-- a) byte substitution
-- Matching each letter in our alphabet with another letter
byteSub :: Char -> Char
byteSub c | c == 'a' = 'h'| c == 'g' = 'b'| c == 'z' = 'a'| otherwise = chr(ord c + 1)


-- b) 
-- Receives an integer x and list of caracters and shifts right
-- the list by x positions
shiftIn :: Int -> [Char] -> [Char]
shiftIn 0 s = s
shiftIn x s = shiftIn (x - 1) ([last s] ++ take ((length s) - 1) s)


-- c)
-- Receives a matrix of characters and returns a matrix in wich for each
-- i^th row in the original matrix the row was a shifted by i entries
shiftRows :: [[Char]] -> [[Char]]
shiftRows m = shiftRows' (length m - 1) m

shiftRows' :: Int -> [[Char]] -> [[Char]]
shiftRows' 0 m = m
shiftRows' n m = shiftRows' (n - 1) (take n m ++ (shiftIn n (m !! n) : (drop (n + 1) m)))

-- d)
-- Receives a function and matrix of characters and returns the encryption for given matrix
roundKey :: (Char -> Char) -> [[Char]] -> [[Char]]
roundKey f [] = []
roundKey f (x:xs) = map f(x) : roundKey f xs

-- e)
-- Receives a function and matrix of characters and returnes and the encrypted String
simpleAES :: (Char -> Char) -> [[Char]] -> String
simpleAES f m = simpleAES' (roundKey f (shiftRows m))

simpleAES' :: [[Char]] -> String
simpleAES' [] = []
simpleAES' (x:xs) = x ++ simpleAES' xs


-- ******************************************************************
-- Problem 2
data JVal = JStr String | JNum Double | JBool Bool | JNull | JObj [(String,JVal)] | JArray [JVal]

-- Receives a JVal and outputs a String representing the given object.
jsonToString :: JVal -> String
jsonToString (JStr x) = x
jsonToString (JNum x) = show x
jsonToString JNull = "null" 
jsonToString (JBool True) = "true"
jsonToString (JBool False) = "false"
jsonToString (JObj (x:xs)) = "{" ++ jsonObj (JObj (x:xs)) ++ "}"
jsonToString (JArray (x:xs)) = "[" ++ jsonArr (JArray (x:xs)) ++ "]"

jsonArr :: JVal -> String
jsonArr (JArray []) = ""
jsonArr (JArray [x]) = (jsonToString x)
jsonArr (JArray (x:xs)) = (jsonToString x) ++ "," ++ jsonArr(JArray xs)

jsonObj :: JVal -> String
jsonObj (JObj []) = ""
jsonObj (JObj [x]) = fst(x) ++ ": " ++ jsonToString (snd x)
jsonObj (JObj (x:xs)) = fst(x) ++ ": " ++ jsonToString (snd x) ++ ", " ++ jsonObj(JObj xs) 


-- ******************************************************************
-- Problem 3
-- a)
-- Returns a reversed list
myReverse :: [a] -> [a]
myReverse = foldl (\xs x -> (x:xs)) []

-- b)
-- Applying f to each element of xs
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:).f) []

-- c)
-- Takes a list of integers and returns a list where every element
-- is negative (keeping the same absolute value)
myNegate :: [Int] -> [Int]
myNegate = foldr (\x xs -> [-abs(x)] ++ xs) []


-- ******************************************************************
-- Problem 4
-- Takes an index n, an item x and list, and returns the given list but with the n'th 
-- element replaced with x.
setElements :: [(Int, a)] -> [a] -> [a]
setElements xs ys =  foldl (\ys x -> (setElements' (fst x) (snd x) ys)) ys xs

setElements' :: Int -> a -> [a] -> [a]
setElements' n x xs = if ((n < length xs) && n >= 0) then (take n xs) ++ [x] ++ (drop (n + 1) xs) else xs