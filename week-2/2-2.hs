import Data.Char

{-

Write a function that, given a string, validates the string as a Finnish IBAN code.

For details, see https://en.wikipedia.org/wiki/International_Bank_Account_Number#Validating_the_IBAN

Length of a Finnish IBAN code is 18. Finnish IBAN begins with the country code FI
and the rest of the characters are digits. You can assume that the input is without whitespaces.
-}
validFinnishIBAN :: [Char] -> Bool
validFinnishIBAN [] = False
validFinnishIBAN [_] = False
validFinnishIBAN full@(x:y:xs) 
    | length full /= 18 = False
    | x /= 'F' || y /= 'I' = False
    | not (containsOnlyDigits xs) = False 
    | otherwise = True


   

-- 1-1
containsOnlyDigits :: [Char] -> Bool
containsOnlyDigits [] = False
containsOnlyDigits (x:xs) = 
    let isLastCall = length xs == 0 
    in
        if isDigit x && isLastCall then True 
        else if isDigit x then True && (containsOnlyDigits xs)
        else False 