module HW02 where
import Words
import Data.List

type Hand = [Char]
type Template = String
type STemplate = Template

-- EXERCISE 1
formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy (x:xs) hand  
	| (x `elem` hand) = formableBy xs (delete x hand)
	| otherwise = False; 

-- EXERCISE 2
wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

-- EXERCISE 3
wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] _ [] = True
wordFitsTemplate [] _ _ = False
wordFitsTemplate _ _ [] = False
wordFitsTemplate (t:template) hand (w:word) 
	| t == '?' && formableBy [w] hand = wordFitsTemplate template (delete w hand) word
	| t == w = wordFitsTemplate template hand word 
	| otherwise = False

-- EXERCISE 4
wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate template hand = [ possibleWord | possibleWord <- wordsFrom (hand ++ template), wordFitsTemplate template hand possibleWord]

-- EXERCISE 5
scrabbleValueWord :: String -> Int
scrabbleValueWord [] = 0
scrabbleValueWord (w:word) = scrabbleValue w + scrabbleValueWord word 

-- EXERCISE 6
bestWords :: [String] -> [String]
bestWords [] = []
bestWords words = [  fst pair | pair <- [ pair | pair <- sortedWordsWithvalues, snd pair == snd (head sortedWordsWithvalues)]] 
	where sortedWordsWithvalues = sortBy wordSorter (matchValues words)

wordSorter :: (String,Int) -> (String,Int) -> Ordering
wordSorter (word,val) (word2, val2) 
	| val < val2 = GT
	| val >= val2 = LT

matchValues :: [String] -> [(String,Int)]
matchValues words = zip words $ map scrabbleValueWord words

-- EXERCISE 7

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate [] _ = 0
scrabbleValueTemplate _ [] = 0
scrabbleValueTemplate template word = wordValue * (calcMultiplier template)
	where wordValue = scrabbleValueTemplateHelper template word

calcMultiplier :: STemplate -> Int
calcMultiplier [] = 1
calcMultiplier template = product ([1] ++ [ read [space] :: Int | space <- template, space == '2' || space == '3'])  


scrabbleValueTemplateHelper :: STemplate -> String -> Int
scrabbleValueTemplateHelper [] _ = 0
scrabbleValueTemplateHelper _ [] = 0
scrabbleValueTemplateHelper (t:template) (w:word) 
	| t == 'D' = ((scrabbleValue w) * 2) + scrabbleValueTemplateHelper template word
	| t == 'T' = ((scrabbleValue w) * 3) + scrabbleValueTemplateHelper template word
	| otherwise = (scrabbleValue w) + scrabbleValueTemplateHelper template word



