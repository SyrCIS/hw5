module Hw5 where


--Nick Walker & Aleksandr Staprans
--CIS 252
--Hwk 5
--This is a homework to test list comp 
--Feb 26, 2014

---------------------------------------------------------------------------------
--1

--CONTRACT
switch :: Char -> Char -> String -> String

--PURPOSE
  -- switch ch1 ch2 str returns the string obtained by replacing every occurrence 
  -- of ch1 in str by ch2 and vice versa. 
  
--EXAMPLES
  -- switch 'h' 'p' "hocus pocus" returns "pocus hocus"
  -- switch 's' 'p' "hocus pocus" returns "hocup socup"
  
--DEFINITION  
switch _ _ [] = []
switch a b (c:cs)
        | (c == b)  = (a:(switch a b cs))
        | (c == a)  = (b:(switch a b cs))
        | otherwise = (c:(switch a b cs))
        
--TESTS
ans1 = switch 'h' 'p' "hocus pocus" -- should be "pocus hocus"
ans2 = switch 's' 'u' "syracuse university" -- should be "uyracsue sniveruity"

-----------------------------------------------------------------------------------        
--2...
        
--CONTRACT       
interleave :: [a] -> [a] -> [a]

--PURPOSE
  -- This should interleave the elements of two lists. The result list should be twice as long as the 
  -- shorter list.
  
--EXAMPLES 
  -- interleave "abcde" "XYZ" returns "aXbYcZ"
  -- interleave "qwerty" "QWERTY" returns "qQwWeErRtTyY"
  
--DEFINITION  
interleave [] _          = []
interleave _ []          = []
interleave (x:xs) (y:ys) = (x:y:(interleave xs ys))

--TESTS
ans3 = interleave "abcde" "XYZ" -- should be "aXbYcZ"
ans4 = interleave "qwerty" "QWERTY" -- should be "qQwWeErRtTyY"

-----------------------------------------------------------------------------------
--3...

--CONTRACT
pairUp :: [a] -> [(a,a)]

--PURPOSE 
  -- pairUp xs returns the list obtained by pairing up the first two elements of xs, then the third and fourth elements
  -- and so on. If xs has an odd number of elements, the final element is paired with itslef.
  
--EXAMPLES
  -- pairUp [3,5,2,9] returns [(3,5),(2,9)]
  -- pairUp "abcde" returns [('a','b'),('c','d'),('e','e')]

--DEFINITION     
pairUp []       = []
pairUp (x:[])   = [(x,x)]
pairUp (x:y:xs) = ((x,y):(pairUp xs))

--TESTS
ans5 = pairUp [3,5,7,9,11] -- should be [(3,5),(7,9),(11,11)]
ans6 = pairUp "test" -- should be [('t','e'),('s','t')]

--------------------------------------------------------------------------------------

--This is an association list for letters in Scrabble
--It will be used in the following problems 4a, 4b

scrabblePoints :: [(Char,Int)]
scrabblePoints = [('a',1), ('b',3), ('c',3), ('d',2), ('e',1), ('f',4), ('g',2), ('h',4), ('i',1), ('j',8), ('k',5), ('l',1), ('m',3), ('n',1), ('o',1), ('p',3), ('q',10), ('r',1), ('s',1), ('t',1), ('u',1), ('v',4), ('w',4), ('x',8), ('y',4), ('z',10)]

---------------------------------------------------------------------------------------
--4a

--CONTRACT
getVal :: Char -> [(Char,Int)] -> Int

--PURPOSE 
  -- This will return the value of character c in the association list ps. 
  -- If the character c is not found in the association list then 0 will be returned 
  -- If there is more than one entry on the first value will be returned. 
  
--EXAMPLES
  -- getVal 'w' scrabblePoints will return 4
  -- getVal 'a' scrabblePoints will return 1
  
--DEFINITION   
getVal _ []        = 0
getVal a ((c,n):ps)
        | (a == c)     = n
        | otherwise    = getVal a ps
        
--TESTS
ans7 = (getVal 'h' scrabblePoints) -- should return 4
ans8 = (getVal 'b' scrabblePoints) -- should return 3

-------------------------------------------------------------------------------------        
--4b

--CONTRACT
scoreWord :: String -> Int

--PURPOSE 
  -- This will return the scrabble scoring of a string. 
  -- It will cycle through the characters in the string and add up the total number of points and return them
  
--EXAMPLES
  -- scoreWord "zowie" will return 17 (10+1+4+1+1)
  -- scoreWord "" will return 0
  
--DEFINITION
scoreWord []     = 0
scoreWord (c:cs) = (getVal c scrabblePoints) + scoreWord cs

--TESTS
ans9 = scoreWord "test" -- should be 4 (1+1+1+1)
ans10 = scoreWord "cis" -- should be 5 (3+1+1)

--------------------------------------------------------------------------------------
--5

--CONTRACT
downgrade :: Char -> Int -> [(Char,Int)] -> [(Char,Int)]

--PURPOSE
  -- This will return a list of pairs such that...
    -- The first pair containing the letter ch is either deleted or has its 
    -- integer component decremented by one
    -- All other pairs in the list are retained 
    
--EXAMPLES
  -- downgrade 'a' 1 [('b',1),('a',3),('c',2),('a',1)] returns [('b',1),('a',2),('c',2),('a',1)]
  -- downgrade 'a' 3 [('b',1),('a',3),('c',2),('a',1)] returns [('b',1),('c',2),('a',1)]
  
--DEFINITION
downgrade _ _ [] = []
downgrade a x ((b,y):ps)
        | (a /= b)   = ((b,y):(downgrade a x ps))
        | (x >= y)   = downgrade a x ps
        | otherwise  = ((b,(y-1)):(downgrade a x ps))

--TESTS
ans11 = downgrade 'a' 1 [('b',1),('a',3),('c',2),('a',1)] --should return [('b',1),('a',2),('c',2),('a',1)]
ans12 = downgrade 'a' 3 [('b',1),('a',3),('c',2),('a',1)] --should return [('b',1),('c',2),('a',1)] 
--It is deleting more than just the first pair containing letter ch, I am not sure how to write a break after it finishes with the first pair. 
    
------------------------------------------------------------------------------------        
--6

--What's to stop this from being general?
--like subsequence :: [a] -> [a] -> Bool  
  --I am not sure if you tried it but you get an error @ b==c it cannot match them      

--CONTRACT
subsequence :: String -> String -> Bool

--PURPOSE
  -- This will test if s1 is a subsequence of s2. This means that you can obtain s1 by deleting a number of characters from s2
  
--EXAMPLES
  -- subsequence "music" "computer science" should return true
  -- subsequence "cat" "dog" should return false
  
--DEFINITION  
--You had True and False mixed up here 
subsequence _ [] = False
subsequence [] _ = True
subsequence (b:bs) (c:cs)
        | (b == c)   = subsequence bs cs
        | otherwise  = subsequence (b:bs) cs

--TESTS
ans13 = subsequence "music" "computer science" -- should return true
ans14 = subsequence "foo" "boofu" -- should return false


