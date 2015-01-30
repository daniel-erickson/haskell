module HW03 where
import Log

--EX 01

parseMessage :: String -> MaybeLogMessage
parseMessage m = case words m of
	("I":ts:msg) 	-> parseInfo ts $ unwords msg 
	("W":ts:msg) 	-> parseWarning ts $ unwords msg 
	("E":ec:ts:msg) -> parseError ec ts $ unwords msg 
	_ 				-> InvalidLM m

parseInfo :: String -> String -> MaybeLogMessage
parseInfo ts msg 
	| isValidInt ts 	= ValidLM $ LogMessage Info (readTimeStamp ts) msg 
	| otherwise 		= InvalidLM msg

parseWarning :: String -> String -> MaybeLogMessage
parseWarning ts msg
	| isValidInt ts 	= ValidLM $ LogMessage Warning (readTimeStamp ts) msg 
	| otherwise 		= InvalidLM msg

parseError :: String -> String -> String -> MaybeLogMessage
parseError ec ts msg
	| (isValidInt ec && isValidInt ts) 	= ValidLM $ LogMessage (Error (readError ec)) (readTimeStamp ts) msg 
	| otherwise 						= InvalidLM msg

isValidInt :: String -> Bool
isValidInt mi = case  (readInt mi) of
	ValidInt i -> True
	InvalidInt -> False
	_ -> False
 
readTimeStamp :: String -> TimeStamp
readTimeStamp ts = read ts :: TimeStamp

readError :: String -> Int
readError ec = read ec :: Int

--EX 02

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly [] = []
validMessagesOnly logs = [log | ValidLM log <- logs]

--EX 03

parse :: String -> [LogMessage]
parse log = validMessagesOnly $ map parseMessage $ lines log  
