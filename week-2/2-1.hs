import Data.Char

{-
Write a function that, given a string, uses recursion on
the list of characters and checks whether the string contains only digits or not.
Empty string should return false.
-}
containsOnlyDigits :: [Char] -> Bool
containsOnlyDigits [] = False
containsOnlyDigits (x:xs) = 
    let isLastCall = length xs == 0 
    in
        if isDigit x && isLastCall then True 
        else if isDigit x then True && (containsOnlyDigits xs)
        else False 