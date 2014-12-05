{-
Name: Daniel Erickson
Collaborators: none
Notes: 
-}

module HW01 where         -- We'll learn more about this later

isThisWorking :: String
isThisWorking = "Yes"
-- Load this file into GHCi (say, with `ghci HW01.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

-- Put your work below.

-- Get the remainder after dividing the number by 10 as many times as we can
-- This is the last digit!!!
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Get the number of times we can divide the number by 10
-- This is the number without the last digit!!!
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Convert the number to a list by appending the last digit as a list to the current list then droping the last digit and going again
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n 
	| n < 0 	= []
	| otherwise = toDigits(dropLastDigit n) ++ [lastDigit n]


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherHelper (reverse xs))

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper []       = []
doubleEveryOtherHelper (x:[])   = [x]
doubleEveryOtherHelper (x:y:zs) = [x,y*2] ++ doubleEveryOtherHelper zs

splitListsOfNumbers :: [Integer] -> [Integer]
splitListsOfNumbers [] 		= [];
splitListsOfNumbers (x:xs) 	=  toDigits x  ++ splitListsOfNumbers xs

sumDigits :: [Integer] -> Integer
sumDigits []	= 0
sumDigits xs 	= sum (splitListsOfNumbers xs)

validate :: Integer -> Bool
validate 0 = False
validate n 
	| 	n < 0 = False
	|	otherwise = (lastDigit . sumDigits . splitListsOfNumbers . doubleEveryOther . toDigits $ n) == 0


type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ 			= []
hanoi n peg1 peg2 peg3 	=  hanoi (n-1) peg1 peg3 peg2 ++ [(peg1,peg2)] ++ hanoi (n-1) peg3 peg2 peg1

hanoi2 :: Integer -> [Peg] -> [Move]
hanoi2 0 _ = []
hanoi2 n (p1:p2:p3:ps)  
	| null ps = 
		hanoi2 (n-1) (p1:p3:p2:ps) 
		++ [(p1,p2)] 
		++ hanoi2 (n-1) (p3:p2:p1:ps)	
	| otherwise = 
		hanoi2 (n `div` 2) (p1:p3:p2:ps) 
		++ hanoi2 (n - (n `div` 2)) (p1:p2:ps) 
		++ hanoi2 (n `div` 2) (p3:p2:p1:ps)

