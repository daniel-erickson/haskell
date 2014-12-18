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


{- 
wordsFittingTemplateFilter :: Template -> String -> Bool
wordsFittingTemplateFilter template word =  wordFitsTemplateHelper template word && (length template == length word)

wordFitsTemplateV1 :: Template -> Hand -> String -> Bool
wordFitsTemplateV1 template hand word
	| length template == length word = word `elem` getPossibleWords
	| otherwise = False 
	where getPossibleWords = [ possibleWord | possibleWord <- wordsFrom (hand ++ getLettersFromTemplate template), wordFitsTemplateHelper template possibleWord]

getLettersFromTemplate :: Template -> Hand
getLettersFromTemplate [] = []
getLettersFromTemplate (x:xy)
	| x == '?' = getLettersFromTemplate xy
	| otherwise = getLettersFromTemplate xy ++ [x]

wordFitsTemplateHelper :: Template -> String -> Bool
wordFitsTemplateHelper [] _ = True
wordFitsTemplateHelper _ [] = True
wordFitsTemplateHelper (x:xs) (y:ys)
	| x == '?' = wordFitsTemplateHelper xs ys
	| x == y = wordFitsTemplateHelper xs ys 
	| otherwise = False

	 -}

