module Assignment1 where

type Year  = Int
type Month = Int
type Day   = Int
type Date  = (Year, Month, Day)
type DayOfYear = Int

isOlder :: Date -> Date -> Bool
isOlder (y1, m1, d1) (y2, m2, d2)
  | y1 < y2   = True
  | m1 < m2   = True
  | d1 < d2   = True
  | otherwise = False

numberInMonth :: [Date] -> Month -> Int
numberInMonth ds m = length $ filter (\(_, m', _) -> m' == m) ds

numberInMonths :: [Date] -> [Month] -> Int
numberInMonths ds ms = length $ filter (\(_, m', _) -> m' `elem` ms) ds

datesInMonth :: [Date] -> Month -> [Date]
datesInMonth ds m = filter (\(_, m', _) -> m' == m) ds

datesInMonths :: [Date] -> [Month] -> [Date]
datesInMonths ds ms = filter (\(_, m, _) -> m `elem` ms) ds

getNth :: [a] -> Int -> a
getNth (x:_) 1 = x
getNth (_:xs) n = getNth xs (n - 1)

months :: [String]
months = ["January", "February", "March", "April", "May", "June",
          "July", "August", "September", "October", "November", "December"]

dateToString :: Date -> String
dateToString (y, m, d) = getNth months m ++ " " ++ show d ++ ", " ++ show y

numberBeforeReachingSum :: Int -> [Int] -> Int
numberBeforeReachingSum b xs = numberBeforeReachingSum' xs 0 0
  where numberBeforeReachingSum' [] _ n = n - 1
        numberBeforeReachingSum' (x:xs') s n
          | s >= b = n - 1
          | otherwise = numberBeforeReachingSum' xs' (s + x) (n + 1)

monthLengths :: [Int]
monthLengths = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

whatMonth :: DayOfYear -> Month
whatMonth d = numberBeforeReachingSum d monthLengths

monthRange :: DayOfYear -> DayOfYear -> [Month]
monthRange d1 d2 = [whatMonth d1 .. whatMonth d2]

older :: Date -> Date -> Date
older d1 d2 = if isOlder d1 d2 then d1 else d2

oldest :: [Date] -> Maybe Date
oldest [] = Nothing
oldest ds = Just $ foldr1 older ds

numberInMonthsChallenge :: [Date] -> [Month] -> Int
numberInMonthsChallenge = numberInMonths

isLeapYear :: Year -> Bool
isLeapYear y
  | y `mod` 400 == 0 = True
  | y `mod` 100 == 0 = False
  | y `mod` 4 == 0 = True
  | otherwise = False

reasonableDate :: Date -> Bool
reasonableDate (y, m, d)
  | y <= 0 = False
  | m `notElem` [1..12] = False
  | isLeapYear y && d <= 29 = True
  | d > monthLengths !! pred m = False
  | otherwise = True
