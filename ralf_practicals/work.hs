import Prelude hiding ( (&&) )

square :: Num a => a -> a
square op = op * op

quad :: Num a => a -> a
quad op = square op * square op

larger :: Ord a => a -> a -> a
larger left right
  | left > right = left
  | otherwise    = right

area :: Double -> Double
area rad = rad * rad * pi

(&&) :: Bool -> Bool -> Bool
(&&) True True = True
(&&) _    _    = False

type Day = Integer
type Month = Integer
type Year = Integer

showDate :: Day -> Month -> Year -> String
showDate day month year = (niceDay day) ++ " " ++ (niceMonth month) ++ " " ++ (show year)

niceDay day = (show day) ++ (daySuffix day)

daySuffix 1 = "st"
daySuffix 2 = "nd"
daySuffix 3 = "rd"
daySuffix otherwise = "th"

niceMonth 1 = "January"
niceMonth 2 = "February"
niceMonth 3 = "March"
niceMonth otherwise = "Don't care"
