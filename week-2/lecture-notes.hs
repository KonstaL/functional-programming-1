{-

-}

-- Function currying is a thing in Haskell
multiply:: Int -> Int -> Int
multiply x y = x*y
multiplyByTwo = multiply 2 

-- Haskell has first class functions
applyTwice f x = f (f x)
-- applyTwice ((*) 3) == 3*3*4

zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys
-- zipWith' (*) [1..] [6,5,4,3]

map' f [] = []
map' f (x:xs) = f x : map' f xs
-- map' ((*) 3) [2..10]

-- filter' _ [] = []
-- filter' p (x:xs) = (p x) : (filter' xs) 

-- Lambdas, or anynomous functions are also a thingß
-- map (\x -> if x<3 then x else 2*x) [1..10]
-- map (\((x,y):xs) -> if x < 3 then x else 2*x) [[(1,3)], [(5,3)]] -- You can pattern match inside lambdas

-- You can fold in Haskell
sum'' xs = foldl (\acc x -> acc + x) 0 xs

-- Dollar sign changes the order of execution
-- sum'' (map (2+) [1..20])
-- sum'' $ map (2+) [1..20]

-- You can combine functions using the dot notation
-- map (negate . abs) [(-3), 2,54]

--You can load modules into ghci using :m <modulename>
-- :d Data.Char
{-

encode shift msg =
    let numbers = map ord msg
        shifted = map ((+) shift) numbers
    in map chr shifted
-}
-- 
