{-
Write a function that is given:
- f, a distance function of type String -> String -> Float (like the ones in Task 3.2)
- d :: Float
- ss :: [String]
For each string in ss it computes a list of similar strings in ss (strings that are at most distance d from the string).

Calling this function with fuction of
3.2a), d=0.3 and ss=["aaabc", "aabdd", "a", "aa", "abdd", "bcbcb", "", "abcdefghij"] should return [["aaabc","aabdd","aa","bcbcb"],["aaabc","aabdd","abdd"],["a","aa"],["aaabc","a","aa"],["aabdd","abdd"],["aaabc","bcbcb"],[""],["abcdefghij"]].
3.2b), d=0.2 and ss=["123a","456789b","45","abc", "ab1", "a12", "abcdefghij"] should return [["456789b","45"],["123a","456789b","45","a12"],["123a","456789b","45","a12"],[],[],["456789b","45"],[]].

Hint: You can use Task3.3 in this task. (You will need to copy-paste one of your functions into the answer of this task.)
Hint: You can load multiple files at the same time using :l file1 file2 in ghci and add them to context using :m + <file1's module name> <file2's module name>. You will have to name your modules, otherwise they are both called Main. This way you can test your answer with functions of Task3.2. (Or you can just copy-paste the functions.)
Note: Make sure not to import your answer for Task3.3 in your answer for this task, since in peer-reviewing the file will not be present.
-}

-- 3.4
myFunc :: (String -> String -> Float) -> Float -> [String] -> [[String]]
myFunc _ _ [] = [];
myFunc f dist  strArr@(x:xs) = [x | x <- (map (\x -> someFunc f dist x strArr) strArr)]
       

-- From 3.3
someFunc :: (String -> String -> Float) -> Float ->  String -> [String] -> [String]
someFunc _ _ _ [] = [];
someFunc f dist str strArr@(x:xs) = [x | x <- strArr, f x str <= dist]


-- From 3.2
a :: String -> String -> Float
a "" "" = 0.0 
a str1 str2 = 
    let charsNotInStr2 = [x | x <- str1, not (elem x str2)]
        charsNotInStr1 = [y | y <- str2, not (elem y str1)]
        in
            fromIntegral((length charsNotInStr1) + (length charsNotInStr2)) / fromIntegral((length str1) + (length str2)) 

-- From 3.2
b :: String -> String -> Float
b "" "" = 0.0 
b str1 str2 = 
    let nonNumChars1 = [x | x <- str1, not (elem x ['0'..'9'])]
        nonNumChars2 = [y | y <- str2, not (elem y ['0'..'9'])]
        in
            fromIntegral(length nonNumChars2 + length nonNumChars1) / fromIntegral(length str1 + length str2) 


