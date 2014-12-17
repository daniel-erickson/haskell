{-
Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:

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

-- Version 1
-- I misunderstood here and wanted to validate it was a valid scrable word as well as making sure it fit the template


-- Version 2

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] _ [] = True
wordFitsTemplate [] _ _ = False
wordFitsTemplate _ _ [] = False
wordFitsTemplate (t:template) hand (w:word) 
	| t == '?' && formableBy [w] hand = wordFitsTemplate template (delete w hand) word
	| t == w = wordFitsTemplate template hand word 
	| otherwise = False




-- EXERCISE 4

wordsFittingTemplateFilter :: Template -> String -> Bool
wordsFittingTemplateFilter [] [] = True
wordsFittingTemplateFilter [] _ = False
wordsFittingTemplateFilter _ [] = False
wordsFittingTemplateFilter (x:xs) (y:ys)
	| x == '?' = wordsFittingTemplateFilter xs ys
	| x == y = wordsFittingTemplateFilter xs ys 
	| otherwise = False

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate template hand = [ possibleWord | possibleWord <- totalHand,  wordsFittingTemplateFilter template possibleWord]
   where totalHand = wordsFrom (hand ++ getLettersFromTemplate template)


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

