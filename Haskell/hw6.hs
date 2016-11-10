
data Date = Date (Int, Int, Int)
  deriving (Show, Eq, Ord)

year :: Date -> Int
year (Date (year, month, day)) = year

month :: Date -> Int
month (Date (year, month, day)) = month

day :: Date -> Int
day (Date (year, month, day)) = day

--isOlder takes two dates, evaluates true if first date is a date and comes
--before second date. Returns false if both dates are the same
isOlder :: Date -> Date -> Bool
isOlder a b
  | a == b = False
  | a < b = True
  | a > b = False


numberInMonth :: [Date] -> Int -> Int
numberInMonth list someMonth = foldl (\acc date -> if month date == someMonth then acc+1 else acc) 0 list

--numberInMonths takes a list of dates and a list of months. Checks to see if the months are featured in the
--list of dates
numberInMonths :: [Date] -> [Int] -> Int
numberInMonths dateList monthList = foldl (\acc date -> if elem (month date) monthList then acc+1 else acc) 0 dateList


--datesInMonth
datesInMonth :: [Date] -> Int -> [Date]
datesInMonth dateList someMonth = filter (\x -> month x == someMonth) dateList

--datesInMonths
datesInMonths :: [Date] -> [Int] -> [Date]
datesInMonths dateList monthList = filter (\x -> elem (month x) monthList) dateList

--getNth
getNth :: [String] -> Int -> String
getNth xs y = xs !! y

--dateToString
dateToString :: Date -> String
dateToString x =
  let stringList = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in stringList !! ((month x)-1) ++ " " ++ show (day x) ++ ", " ++ show (year x)

--numberBeforeReachingSum
numberBeforeReachingSum :: Int -> [Int] -> Int
numberBeforeReachingSum theSum intList = length (filter (< theSum ) (scanl1 (\acc item -> (acc + item))intList))

--whatMonth
whatMonth :: Int -> Int
whatMonth x =
  let monthList = [31,28,31,30,31,30,31,31,30,31,30,31]
  in numberBeforeReachingSum x monthList

--monthRange
monthRange :: Int -> Int -> [Int]
monthRange dayStart dayEnd = let
  dayList = [dayStart..dayEnd]
  in
  map (\x -> whatMonth x) dayList

--oldest
oldest :: [Date] -> Maybe Date
oldest [] = Nothing
oldest x = Just (minimum x)


--validDate
validDate :: Date -> Bool
validDate x = let
  d = day x
  m = month x
  y = year x
  monthList = [31,28,31,30,31,30,31,31,30,31,30,31]
  in (
  ((d <= (monthList !! (m-1))) || (d <= 29 && m == 2 && ((y `mod` 400 == 0) || (y `mod` 4 == 0 && y `mod` 100 /= 0 )))) && (m <= 12) && (y > 0)
  )
