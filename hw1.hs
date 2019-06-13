
-- Question 1
-- A function wich takes a String and returns True if the input String 
-- is a palindrome and False otherwise.
isPali :: String -> Bool
isPali xs = (xs == reverse(xs))


-- Question 2
-- A function which takes 2 Strings and returns True if the first String
-- is a prefix of the second String, and False otherwise.
prefix :: String -> (String -> Bool)
prefix "" _ = True
prefix _ "" = False
prefix (x:xs) (y:ys) = if(x == y) then prefix xs ys 
						else False


-- Question 3.a
-- The function takes an Int n abd produces a list of the first n integers
-- doubled in ascending order using the operator ++
doubleList :: Int-> [Int]
doubleList 0 = [0]
doubleList x = doubleList(x - 1) ++ [2 * x]


-- Question 3.b
-- The function takes an Int n abd produces a list of the first n integers
-- doubled in descending order
listDouble :: Int -> [Int]
listDouble n = reverse(doubleList n)


-- Question 4
-- Calculating the Greatest Common Divisior of 2 numbers
myGcd :: Int -> (Int -> Int)
myGcd 0 y = y
myGcd y 0 = y
myGcd x y = let a = compInt x y in if (last a `mod` head a == 0) 
										then head a
			else myGcd (head a) (last a `mod` head a)

-- Auxiliary function for Question 4 to compare Integers
compInt :: Int -> (Int -> [Int])
compInt x y = if (x < y) then [x, y] 
				else [y, x]

-- Question 5.a
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = if (x < 0) then [] 
				else toDigits(x `div` 10) ++ [x `mod` 10]


-- Question 5.b
doubleIntermittent :: [Integer] -> [Integer]
doubleIntermittent [] = []
doubleIntermittent (x:xs) = if (length(x:xs) `mod` 2 == 0) then 2 * x : doubleIntermittent(xs)
								else x : doubleIntermittent(xs)


-- Question 5.c
sumDigitsInList :: [Integer] -> Integer
sumDigitsInList [] = 0
sumDigitsInList (x:xs) = sum(toDigits(x)) + sumDigitsInList(xs)


-- Question 5.d
vallidate :: Integer -> Bool
vallidate x = (sumDigitsInList(doubleIntermittent (toDigits(x)))) `mod` 10 == 0


-- Question 6.a
mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum(xs)


-- Question 6.b
mySumTail :: [Int] -> Int 
mySumTail (x:xs) = mySumTail_acc (x:xs) 0

mySumTail_acc :: [Int] -> (Int -> Int)  
mySumTail_acc [] acc = acc
mySumTail_acc (x:xs) acc = x + mySumTail_acc xs acc


-- Question 7
-- The function takes an Integer and checks if it is a perfect numbers
perfect :: Integer -> Bool
perfect 0 = True
perfect x = perfect_acc x 1 x

perfect_acc :: Integer -> Integer -> Integer -> Bool
perfect_acc 1 acc 1  = False
perfect_acc x acc y = if (acc < x) 
							then if (x `mod` acc == 0 || acc == 1) then perfect_acc (x) (acc + 1) (y - acc) 
						else perfect_acc x (acc + 1) y 
						else if (y == 0) then True 
						else False 

