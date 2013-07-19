module Assignment2 where

import Prelude hiding (concat, elem, map)

--- Helper functions (Less leverage of Prelude)
elem :: (Eq a) => a -> [a] -> Bool
elem _ [] = False
elem a (x:xs)
    | a == x = True
    | otherwise = a `elem` xs

delete :: (Eq a) => a -> [a] -> [a]
delete _ [] = []
delete a (x:xs)
    | x == a = delete a xs
    | otherwise = x : delete a xs

concat :: [[a]] -> [a]
concat = foldr (++) []

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
                      Just x' -> x' : mapMaybe f xs
                      Nothing -> mapMaybe f xs

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
---

--- Problem 1
data FullName = FullName String String String deriving (Show)

allExceptOption :: (Eq a) => a -> [a] -> Maybe [a]
allExceptOption x xs = if x `elem` xs then Just $ delete x xs else Nothing

getSubstitutions1 :: (Eq a) => [[a]] -> a -> [a]
getSubstitutions1 [] _ = []
getSubstitutions1 (x:xs) s = case allExceptOption s x of
                               Just x' -> x' ++ getSubstitutions1 xs s
                               Nothing -> getSubstitutions1 xs s

getSubstitutions2 :: (Eq a) => [[a]] -> a -> [a]
getSubstitutions2 xs s = concat . mapMaybe (allExceptOption s) $ xs

similarNames :: [[String]] -> FullName -> [FullName]
similarNames xs (FullName f m l) = map (\f' -> FullName f' m l) subs
    where subs = getSubstitutions2 xs f
---

--- Problem 2
data Suit = Spades | Clubs | Diamonds | Hearts deriving (Show, Read, Eq)
data Color = Black | Red deriving (Show, Eq)
data Rank = Jack | Queen | King | Ace | Num Int deriving (Show, Eq)
data Card = Card Suit Rank deriving (Show, Eq)

data Move = Draw | Discard Card

cardColor :: Card -> Color
cardColor (Card s _) = case s of
                         Spades -> Black
                         Clubs -> Black
                         _ -> Red

cardValue :: Card -> Int
cardValue (Card _ r) = case r of
                         Ace -> 11
                         (Num i) -> i
                         _ -> 10

removeCard :: [Card] -> Card -> [Card]
removeCard []  _ = []
removeCard (x:xs) c
    | x == c = xs
    | otherwise = x : removeCard xs c

allSameColor :: [Card] -> Bool
allSameColor (x:xs) = allSameColor' (cardColor x) $ map cardColor xs
    where allSameColor' _ [] = True
          allSameColor' c (y:ys)
              | c /= y = False
              | otherwise = allSameColor' c ys

allSameColorBetter :: [Card] -> Bool
allSameColorBetter (c:cs) = all (== color) $ map cardColor cs
    where color = cardColor c

sumCards :: [Card] -> Int
sumCards = sum . map cardValue

score :: [Card] -> Int -> Int
score xs goal
    | allSameColorBetter xs = prelimscore `div` 2
    | otherwise = prelimscore
    where initsum = sumCards xs
          prelimscore = if initsum > goal
                          then 3 * (initsum - goal)
                          else goal - initsum

officiate :: [Card] -> [Move] -> Int -> Int
officiate cs ms g = officiate' cs ms []
  where officiate' _ [] hs' = score hs' g
        officiate' cs' (m':ms') hs' = case m' of
                                    Discard c -> officiate' (removeCard cs' c) ms' hs'
                                    _ -> case cs' of
                                          [] -> score hs' g
                                          (c'':cs'') -> let newHand = c'':hs'
                                                            newScore = score newHand g
                                                    in if newScore > g
                                                        then newScore
                                                        else officiate' cs'' ms' newHand
---
