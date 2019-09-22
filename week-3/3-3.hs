{-
Write a function that is given:
- f, a distance function of type String -> String -> Float (like the ones in Task 3.2)
- d :: Float
- z :: String
- ss : [String]
Your function makes a list of strings w in ss such that the distance between w and z is at most d (as calculated with f, given as a parameter to your function).

Do this by
(a) basic recursion
(b) list comprehension
(c) foldl
(d) using the filter function

Calling these functions with fuction of
3.2a), d=0.3, z="aaabc" and ss=["aabdd","a","aa","abdd","bcbcb","", "abcdefghij"] should return ["aabdd","aa","bcbcb"].
3.2b), d=0.2 ,z="123a" and ss=["456789b","45","abc", "ab1", "a12", "abcdefghij"] should return ["456789b","45"].

Hint: You can load multiple files at the same time using :l file1 file2 in ghci and add them to context using :m + <file1's module name> <file2's module name>. You will have to name your modules, otherwise they are both called Main. This way you can test your answer with functions of Task3.2. (Or you can just copy-paste the functions.)

Note: Make sure not to import your answer for Task3.2 in your answer for this task, since in peer-reviewing the file will not be present.
-}

someFuncA :: (String -> String -> Float) -> Float ->  String -> [String] -> [String]
someFuncA _ _ _ [] = [];
someFuncA f dist str strArr@(x:xs) = if((f x str) <= dist) 
        then [x] ++ (someFuncA f dist str xs)
        else someFuncA f dist str xs   


someFuncB :: (String -> String -> Float) -> Float ->  String -> [String] -> [String]
someFuncB _ _ _ [] = [];
someFuncB f dist str strArr@(x:xs) = [x | x <- strArr, f x str <= dist]

someFuncC :: (String -> String -> Float) -> Float ->  String -> [String] -> [String]
someFuncC _ _ _ [] = [];
someFuncC f dist str strArr@(x:xs) =  foldl (\acc x -> if f x str <=dist then acc ++ [x] else acc) [] strArr

someFuncD :: (String -> String -> Float) -> Float ->  String -> [String] -> [String]
someFuncD _ _ _ [] = [];
someFuncD f dist str strArr@(x:xs) = filter (\x -> f x str <=dist) strArr


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


