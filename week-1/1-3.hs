{-
We represent playing cards with (Char,Int) pairs. 's' means spades, 'h' hearts, 'c' clubs and  'd' diamonds, with number values going from 2 to 14 (Ace being 14). Consider a game where a player gets two cards, and your function needs evaluate how many credits the player wins. The credits are determined as follows:

If the player has the Ace of Spades, that is ('s',14) then the player wins 14 credits. 
Otherwise if the player has  two consecutive numbes of the same suit (spades, hearts, clubs or diamonds), then the player wins 8 credits.
Otherwise if the player has a pair (same number values), then the player wins 6 credits.
Otherwise if the player has two consecutive numbers, then the player wins 4 credits.
Otherwise if the player has two cards of the same suit (spades, hearts, clubs or diamonds) then the player wins 2 credits.
Otherwise, the player wins 0 credits.
E.g. if the function is called with parameters

('c',8) ('c',7)

Then it should return 8
-}
cardGameScore :: (Char, Int) -> (Char, Int) -> Int
cardGameScore (chr, score) (chr2, score2)
    | not (elem chr "cdhs") || not (elem chr2 "cdhs")  = error "Invalid Character! Must be either 'c' 'd' 'h' or 's'!"
    | not (elem score [2..14]) || not (elem score2 [2..14]) =  error "Invalid score! Score must be between 2 and 14"
    | (chr == 's' && score == 14) || (chr2 == 's' && score2 == 14)  = 14
    | chr == chr2 && (score + 1  == score2 || score2 + 1 == score) = 8
    | score == score2 = 6
    | score + 1  == score2 || score2 + 1 == score = 4
    | chr == chr2 = 2
    | otherwise = 0


