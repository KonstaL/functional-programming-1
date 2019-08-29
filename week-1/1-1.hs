-- Find all pairs where manhattan distance is  x
findManhattanCoordinates :: Int -> [(Int,Int)]
findManhattanCoordinates x = [(a, b) | a <- take (x + 1) [0..], b <- take (x +1) [0..], a + b == x]
