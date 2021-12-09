

import Text.Printf ( printf )


-- This function takes a Double and rounds it to 2 decimal places.
getRounded :: Double -> Double
getRounded x = read s :: Double
               where s = printf "%.2f" x


-- This function takes a Double value and a String representing
--the name of currency which the given money in Turkish Liras will be converted to.
convertTL :: Double -> String -> Double
convertTL money currency = getRounded (money / exchange currency)
exchange "USD" = 8.18
exchange "EUR" = 9.62
exchange "BTC" = 473497.31

--This function takes a list of Strings as
--the schedule of the turns, a String representing the name of a certain employee and an Int representing
--the some number of days. This function shall calculate the number of watches assigned to this employee
--up to given days according to the arranged schedule.

countOnWatch :: [String] -> String -> Int -> Int
countOnWatch schedule employee days = length [x | x<-take days schedule,x==employee]

--This function takes an Int with 4 digits as the password of the
--customer and returns an Int as the encrypted password by processing the digits in the original one.
--Encryption rules:
--If the digit is divisible by 3, it is subtracted by 1.
--If the digit is divisible by 4, it is multiplied by 2.
--If the digit is divisible by 5, it is incremented by 3.
--For the rest of the cases, the digit is incremented by 4..
--If any of the above operation results in a double digit number, the number in the ones place is taken (e.g. 6 for 16.).

encrypt :: Int -> Int
encrypt x = total (map process (parseNumber x)) 3
parseNumber x = [x `div` 1000,x `mod` 1000 `div` 100,x `mod` 100 `div` 10,x `mod` 10]
process x
    | x `mod` 3 == 0 = (x-1) `mod` 10
    | x `mod` 4 == 0 = x*2 `mod` 10
    | x `mod` 5 == 0 = (x+3) `mod` 10
    | otherwise = (x+4) `mod` 10
total [] _ = 0
total (x:xs) digit = x*10^digit + total xs (digit-1)



--This function takes a list of (Double, Int) pairs as the money and number of years for the investment of 
--each customer. The function returns a list of Double as the total money that each customer
--would have for their investment.

--Formula for the calculation of total money with compound interest: T = P x (1 + R/12)^(12 x N)
--where T is total money, P is the initial money, R is the annual interest rate and N is the number of years.

--Annual interested is decided by the rules below:
--If money >= 10000, if years >= 2 then R = 11.5% else R = 10.5%
--If money < 10000, if years >= 2 then R = 9.5% else R = 9.0%

compoundInterests :: [(Double, Int)] -> [Double]
compoundInterests investments = [getRounded (totalMoney (x,y))| (x,y)<-investments]

moneycalc initial rate years = initial * (1+rate/12)^(years*12)
totalMoney (money, year)
  | year >= 2 =
      if money >= 10000 then moneycalc money 0.115 year
     else moneycalc money 0.095 year
  | money>=10000 = moneycalc money 0.105 year
  | otherwise = moneycalc money 0.09 year