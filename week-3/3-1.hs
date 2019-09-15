{-
Let us number the smaller case characters from 'a' to 'z' with numbers starting from 1, that is, 'a' is given number 1, 'b' is given number 2, etc.

Give functions to compute the following:
- characters that have an odd number. This should return "acegikmoqsuwy".
- characters that have a number that is a product of two odd positive integers x and y, where x/=1 and y/=1. This should return "iouy".

Note: Your functions needs to actually compute these values.
-}

import Data.List

getOddNumChars :: String
getOddNumChars = [ x | (x, y) <- paired, odd y  ]
    where 
        paired = zip ['a'..'z'] [1..] :: [(Char, Int)]

getOddNumProductChars :: String
getOddNumProductChars = nub [ x | (x, y) <- paired, odd1 <-take 20 [3,5 ..], odd2 <- take 20 [3,5 ..], y == odd1*odd2   ]
    where 
        paired = zip ['a'..'z'] [1..] :: [(Char, Int)]