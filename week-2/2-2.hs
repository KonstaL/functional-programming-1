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
    | otherwise = 
        let (ib1, ib2) = splitAt 4 full
            shifted = ib2 ++ ib1
            mapped = concat (map chartToIbanInt shifted) :: String
        in
            if mod (read mapped) 97 == 1 then True
            else False



chartToIbanInt :: Char -> String
chartToIbanInt n
    | isAlpha n = show (ord (toUpper n) - ord 'A' + 10)
    | isDigit n = [n]
    | otherwise = error (n : " is not an IBAN character!")

-- 1-1
containsOnlyDigits :: [Char] -> Bool
containsOnlyDigits [] = False
containsOnlyDigits (x:xs) = 
    let isLastCall = length xs == 0 
    in
        if isDigit x && isLastCall then True 
        else if isDigit x then True && (containsOnlyDigits xs)
        else False 