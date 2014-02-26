--1...

switch :: Char -> Char -> String -> String
switch _ _ [] = []
switch a b (c:cs)
	| (c == b)  = (a:(switch a b cs))
	| (c == a)  = (b:(switch a b cs))
	| otherwise = (c:(switch a b cs))
--2...
	
interleave :: [a] -> [a] -> [a]
interleave [] _          = []
interleave _ []          = []
interleave (x:xs) (y:ys) = (x:y:(interleave xs ys))

--3...

pairUp :: [a] -> [(a,a)]
pairUp []       = []
pairUp (x:[])   = [(x,x)]
pairUp (x:y:xs) = ((x,y):(pairUp xs))

-- This will be useful for tests... 
-- not sure where she wants us to put it in the code though

scrabblePoints :: [(Char,Int)]
scrabblePoints = [('a',1), ('b',3), ('c',3), ('d',2), ('e',1), ('f',4), ('g',2), ('h',4), ('i',1), ('j',8), ('k',5), ('l',1), ('m',3), ('n',1), ('o',1), ('p',3), ('q',10), ('r',1), ('s',1), ('t',1), ('u',1), ('v',4), ('w',4), ('x',8), ('y',4), ('z',10)]

--4...
	--a
getVal :: Char -> [(Char,Int)] -> Int
getVal _ []        = 0
getVal a ((c,n):ps)
	| (a == c)     = n
	| otherwise    = getVal a ps
	--b
scoreWord :: String -> Int
scoreWord []     = 0
scoreWord (c:cs) = (getVal c scrabblePoints) + scoreWord cs

--5...

downgrade :: Char -> Int -> [(Char,Int)] -> [(Char,Int)]
downgrade _ _ [] = []
downgrade a x ((b,y):ps)
	| (a /= b)   = ((b,y):(downgrade a x ps))
	| (x >= y)   = downgrade a x ps
	| otherwise  = ((b,(y-1)):(downgrade a x ps))

	
--6...
--What's to stop this from being general?
--like subsequence :: [a] -> [a] -> Bool	

subsequence :: String -> String -> Bool
subsequence _ [] = True
subsequence [] _ = False
subsequence (b:bs) (c:cs)
	| (b == c)   = subsequence bs cs
	| otherwise  = subsequence (b:bs) cs

