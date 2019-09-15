{-
Write the following functions that measure distance between two strings x and y. Please note that they are not standard well-behaving distance functions.

Notice that e.g. length of a list is an Int and you can get a fractional (non-integer) value out of that with the fromIntegral function.

The type of both functions should be: String -> String -> Float

a)
( (count of how many of the characters in x do not appear in y) + (count of how many of the characters in y do not appear in x)) / ((length of x) + length of y))
If both lists are empty, then the distance is 0.

For example, the distance between "aaabc" and "aabdd" with this function is (1 + 2) / (5 + 5).
In "aaabc" 'a' and 'b' appear in "aabdd" but 'c' does not so we get 1; in "aabdd" 'a' and 'b' appear in "aaabc" but 'd' does not so we get 2; both "aaabc" and "aabdd" are of length 5.

b)
(count of characters in x that are other than any of '0'..'9') + (count of characters in y that are other than any of '0'..'9') / ((length of x) + (length of y))
If both lists are empty, then the distance is 0.

For example for "xy765" and "abc2311" the result is (2+3)/(5+7)
-}


a :: String -> String -> Float
a "" "" = 0.0 
a str1 str2 = 
    let charsNotInStr2 = [x | x <- str1, not (elem x str2)]
        charsNotInStr1 = [y | y <- str2, not (elem y str1)]
        in
            fromIntegral((length charsNotInStr1) + (length charsNotInStr2)) / fromIntegral((length str1) + (length str2)) 


b :: String -> String -> Float
b "" "" = 0.0 
b str1 str2 = 
    let nonNumChars1 = [x | x <- str1, not (elem x ['0'..'9'])]
        nonNumChars2 = [y | y <- str2, not (elem y ['0'..'9'])]
        in
            fromIntegral(length nonNumChars2 + length nonNumChars1) / fromIntegral(length str1 + length str2) 

