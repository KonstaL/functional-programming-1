{-
Let us number the smaller case characters from 'a' to 'z' with numbers starting from 1, that is, 'a' is given number 1, 'b' is given number 2, etc.

Give functions to compute the following:
- characters that have an odd number. This should return "acegikmoqsuwy".
- characters that have a number that is a product of two odd positive integers x and y, where x/=1 and y/=1. This should return "iouy".

Note: Your functions needs to actually compute these values.
-}

getOddNumChars :: String -> String
getOddNumChars "" = "";
getOddNumChars str@(x:xs) = [ x | x <- str, let y = (lookup x paired) , y /= Nothing, let z =  (Just y), odd z]
    where 
        paired = zip ['a'..'z'] [1..] :: [(Char, Int)]
        mappadStr = map (\x -> lookup x paired) str