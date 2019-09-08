{-
We say that character pair (c1,c2) appears in string s with gap g,
if c1 is before c2 and there are exactly g characters between c1 and c2 in s.
Write a function that, given a pair (c1,c2) a gap g, and a string s,
returns an Int telling how many times (c1,c2) appear in s with gap g.

E.g. ('a','b') with gap 1 appears 2 times in “aaabbb".

Use recursion.
-}

findPairsWithGap :: (Char, Char) -> Int -> String -> Int
findPairsWithGap (_,_) _ "" = 0
findPairsWithGap (c1, c2) gap str@(x:xs) 
    | gap < 0 = error "There can't be a negative gap!"
    | gap > length xs -1 = 0
    | x == c1 && (xs !! gap) == c2 = 1 + findPairsWithGap (c1, c2) gap xs
    | otherwise =  0 + findPairsWithGap (c1, c2) gap xs

