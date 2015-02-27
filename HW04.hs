{-
Name: Daniel Erickson
Collaborators: none
Notes: 
-}

module HW04 where         

ex1 :: a -> b -> b
ex1 x y = y
-- We dont get much choice here, return y or die

ex2 :: a -> a -> a
ex2 x y = x
-- we could return x or y here!

ex3 :: Int -> a -> a
ex3 x y = y
-- I could not find another safe implementation without knowing what the type a is

ex4 :: Bool -> a -> a -> a
ex4 test x y 
	| test == True  = x 
	| test == False = y
-- This one has a couple of implementations but this seems like the only one that does anything useful

ex5 :: Bool -> Bool
ex5 test1 = True
-- Knowing the type we can do a bit more here, lets confuse people by always returning True even when i tell it False!  

ex6 :: (a -> a) -> a
ex6 f = error "Impossible"
-- Im not entirely sure about this but i cant see a way to write something that will compile,

ex7 :: (a -> a) -> a -> a
ex7 f x = x
-- Ok so this one compiles it is just applying a function to x

ex8 :: [a] -> [a]
ex8 x = x
-- take in a list of a, output a list of a

ex9 :: (a -> b) -> [a] -> [b]
ex9 f [] = []
ex9 f (x:yz) = [f x] ++ ex9 f yz 
-- I think you could do a bunch of interesting things here to a list (x:yz)
-- For example you could double every item in a list with ex9 (*2) [1,2,3,4,5]

ex10 :: Maybe a -> a
ex10 x = error "My Maybe is rusty but i dont think there is anyway to implement this"

ex11 :: a -> Maybe a
ex11 x = Just x
-- Its just X

ex12 :: Maybe a -> Maybe a
ex12 x = x
-- Maybe X is Maybe X?

