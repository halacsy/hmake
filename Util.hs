{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Util 
where
import Data.Time.Calendar

newtype PDay =  PDay Integer deriving (Eq, Ord, Enum, Num)
newtype PMonth = PMonth Int deriving (Eq, Ord)

instance Show PDay where
    show (PDay i) = show i 

instance Show PMonth where
    show (PMonth i) = show i
{-
instance Num PDay where
  (+) (PDay i) (PDay j) = PDay $ i + j -- could be functor?
  (*) = undefined
  (-) = undefined
  negate = undefined
  abs = undefined
  signum  = undefined
  fromInteger  = PDay

instance Enum PDay where
    succ (PDay i) = PDay (succ i)
    pred (PDay i) = PDay (pred i)
    toEnum = undefined
    fromEnum = undefined
    enumFrom = undefined
    enumFromThen = undefined
    enumFromTo = undefined
    enumFromThenTo = undefined
-}
dayOfBirth::Day
dayOfBirth = fromGregorian 2009 4 5
(y0, m0, d0) = toGregorian dayOfBirth 

 --days_of_month y m = [1 .. gregorianMonthLength (fromIntegral y) m]
daysOfMonth::PMonth -> [PDay]
daysOfMonth pm =  map (pDayFromGregorian y m) [1 .. gregorianMonthLength y m]
    where
        (y, m) = gregorianFromPMonth pm



pDayFromGregorian::Integer->Int->Int->PDay
pDayFromGregorian y m d = PDay $ diffDays (fromGregorian y m d)  dayOfBirth

pMonthFromGregorian::Integer-> Int->PMonth
pMonthFromGregorian y1 m1  = PMonth $ (fromIntegral (y1 - y0)) * 12 + (m1 - m0)

gregorianFromPDay::PDay->(Integer, Int, Int)
gregorianFromPDay (PDay d) = toGregorian $ addDays d dayOfBirth


gregorianFromPMonth::PMonth->(Integer, Int)
gregorianFromPMonth (PMonth m) = (y0 + (fromIntegral (m `div` 12)), m0+ m `mod` 12)

pDayToJulian::PDay->Day 
pDayToJulian (PDay i) = addDays i dayOfBirth 

showPDayAsGregorian::PDay->String
showPDayAsGregorian = showGregorian . pDayToJulian

{- this is for quickcheck -}
prop_pMonthIsomorph::Int->Bool
prop_pMonthIsomorph m =  pid m' == m'
    where pid m =  let (y, m) = gregorianFromPMonth m' in pMonthFromGregorian y m
          m' = PMonth m

prop_pDayIsomorph::Integer -> Bool
prop_pDayIsomorph d = pid d' == d'
    where pid d =  let (y, m, d) = gregorianFromPDay d' in pDayFromGregorian y m d
          d' = PDay d

{- 
main = do 
    print $  showPDayAsGregorian $  pDayFromGregorian 2012 8 24

-}
