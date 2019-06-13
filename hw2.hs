
-- Problem 1.a
-- A function takes an uncurred function and converts the 
-- function into a function of the second kind(curried)
myCurry  :: ((t1, t2)->t)->t1->t2->t
myCurry f x y = f(x, y)

-- Problem 1.b
-- a function performs the opposite 
myUncurry :: (t1->t2->t)->(t1, t2)->t
myUncurry f(x, y) = f x y


-- Problem 2
bundler :: Eq a => [a] -> [[a]]
bundler [] = []
bundler [x] = [[x]]
bundler (x:xs) = [bundlerSub x (x:xs)] ++ bundler (drop (lengthi x xs) xs)

lengthi :: Eq a => a-> [a] -> Int
lengthi xs n = length (bundlerSub xs n)

bundlerSub :: Eq a => a -> [a] -> [a]
bundlerSub n [] = []
bundlerSub n (x:xs) = if x == n then x : bundlerSub n xs else []


-- Problem 3
-- Here we will use the helper function from problem 2
numAppearances :: Eq t => [t]->[(Int,t)]
numAppearances [] = []
numAppearances (x:xs) = zip [lengthi x (x:xs)] [x] ++ numAppearances (drop (lengthi x xs) xs)


-- Problem 4
-- Drops every n'th element in the list
dropMod :: [a] -> Int -> [a]
dropMod [] n = []
dropMod (x:xs) n = take (n-1) (x:xs) ++ dropMod (drop n (x:xs)) n


-- Problem 5
-- Converts a decimal into a binary number
toBinary :: Integral a => a->[a]
toBinary 0 = [0]
toBinary 1 = [0,1]
toBinary x = 0 : correctRes (toBinary' x 31 x)

toBinary' :: Integral a => a -> Int -> a -> [a]
toBinary' x (-1) n = []
toBinary' x acc sum = if (sum == 0) then [] 
						else if (x > 2^acc) then 1 : toBinary' (x-2^acc) (acc-1) (sum-(2^acc)) 
						else if (x == 2^acc) then 1 : addZero(acc) 
						else 0 : toBinary' x (acc - 1) (sum)

addZero :: Integral a => Int -> [a]
addZero (-1) = []
addZero 0 = []
addZero x = [0] ++ addZero (x - 1)

correctRes :: Integral a => [a] -> [a]
correctRes (x:xs) = if (x==0) then correctRes(xs) else (x:xs)


-- Problem 6.a
insertItem :: (String, a) -> [(String,a)] -> [(String,a)]
insertItem a (x:xs) = a : (x:xs)

-- Problem 6.b
itemsByKey :: String -> [(String,a)] -> [a]
itemsByKey a [] = []
itemsByKey a (x:xs) = if fst(x) == a then snd x : itemsByKey a xs 
						else itemsByKey a xs

-- Problem 6.c
getKeys :: [(String,a)] -> [String]
getKeys [] = []
getKeys (x:xs) = getKeys' (fst(x) : getKeys(xs))

getKeys' :: [String] -> [String]
getKeys' [] = []
getKeys' (x:xs) = if (length (filter(== x) (x:xs)) > 1) then x : getKeys' (filter (/=x) xs) 
					else x : getKeys' xs

-- Problem 6.d
groupItemsByKey :: [(String, a)] -> [(String,[a])]
groupItemsByKey [] = []
groupItemsByKey (x:xs) = groupItemsByKey' (zip [fst(x)] [itemsByKey (fst (x)) (x:xs)] ++ groupItemsByKey xs) []

groupItemsByKey' :: [(String,[a])] -> [String] -> [(String,[a])]
groupItemsByKey' [] b = []
groupItemsByKey' (x:xs) b = if (elem (fst(x)) b) then groupItemsByKey' (xs) (b) 
							else [x] ++ groupItemsByKey' (xs) (fst(x):b)


-- Problem 7
bar :: (a -> (b -> c) -> c)-> ((a -> c) -> c) -> (b -> c) -> c
bar x y f = y (flip x f)

foo ::(a -> (b, c)) -> (b -> d) -> (c -> d -> e) -> a -> e
foo x y f a = f (snd (x a)) (y (fst(x a)))



