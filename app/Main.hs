module Main where

import Lib
import GHC.Float
import Data.List.Split ( chunksOf, splitOn )
import Data.List
import Data.Time ( defaultTimeLocale, parseTimeM, Day )
import Data.Maybe ( isJust )

main :: IO ()
main = menuLoop

-- Diplay menu
menu = do
    putStrLn "\nWeather Average Temperature App"
    putStrLn "[1] Average temperature by day"
    putStrLn "[2] Average temperature by week"
    putStrLn "[3] Total temperature average of all days"
    putStrLn "[4] Exit"
    putStrLn "Choose one"
    getLine

-- Exit option
exitMenu :: IO ()
exitMenu = putStrLn "See ya!"

-- Pattern matching of options with its output
menuSelection :: String -> IO ()
menuSelection "1" = dailyAvg >> menu >>= menuSelection
menuSelection "2" = weeklyAvg  >> menu >>= menuSelection
menuSelection "3" = totalAvg >> menu >>= menuSelection
menuSelection "4" = exitMenu

-- Recursive menu
menuLoop = menu >>= menuSelection

-- Total average function
totalAvg :: IO ()
totalAvg = getData >>= \x -> totalAvgOutput (filtering x)

-- Daily Average Function
dailyAvg :: IO ()
dailyAvg = getData >>= \x -> dailyAvgOutput (filtering x)

-- Weekly Average Function
weeklyAvg :: IO ()
weeklyAvg = getData >>= \x -> weeklyAvgOutput (filtering x)

-- Get data by reading the CSV file
getData = readFile "temperature-data.csv"

-- List of days and weeks
dayArray = ["11th of May 2020 ", "12th of May 2020 ", "13th of May 2020 ", "14th of May 2020 ", "15th of May 2020 ", "16th of May 2020 ", 
            "17th of May 2020 ", "18th of May 2020 ", "19th of May 2020 ", "20th of May 2020 ", "21st of May 2020 ", "22nd of May 2020 ",
            "23rd of May 2020 ", "24th of May 2020 "]
weekArray = ["Week 1 - 11th May -> 17th May ", "Week 2 - 18th May -> 24th May "]

-- Filter our with proper format
filtering x =
    let fixedData = (formattedData x)
    -- Creates a list of days
        dayList = uniqueDays fixedData
        dayAverages = map (\x -> dayData x fixedData) dayList
        daysAndAverages = zip dayList dayAverages
    in 
    dayAverages

-- Total average function
totalAvgOutput x = do
    let totalAverage = average x
    putStrLn "\nTotal Average Temperature:"
    putStrLn (show totalAverage ++ " Celcius")

-- Daily average function
dailyAvgOutput x = do
    let dailyAverage = x
        dailyAverageWithDate = zip dayArray dailyAverage
    putStrLn "\nDaily Average Temperatures:"
    mapM_ print dailyAverageWithDate

-- Weekly average output
weeklyAvgOutput x = do
    let weeklyAverage = apply average (chunksOf 7 x)
        weeklyAverageWithWeek = zip weekArray weeklyAverage
    putStrLn "\nWeekly Average Temperatures:"
    mapM_ print weeklyAverageWithWeek

-- Proper formatting of data
formattedData s = map (splitOn ",") (drop10 (lines s))

-- Drops first 10 elements in  raw data
drop10 :: [x] -> [x]
drop10  x = drop 10 x

-- Sorts the temperatures by the day in the timestamp
sortByDay y xs = [double | [date, double] <- xs, y `isPrefixOf` date] 
dayData x result = average . map toDouble . sortByDay x $ result

-- Sorting and creating List of Days
listOfDays :: [[x]] -> [x] -- Make a new list of days
listOfDays xs = [date | [date, double] <- xs]
properDays :: [[x]] -> [[x]] -- Removes the last 5 characters from the TimeStamp
properDays = map (\l -> take (length l - 5) l)
unique :: Eq a => [a] -> [a] --Uniquefication 
unique x = nub x

-- Make a list of unique days
uniqueDays :: Eq x => [[[x]]] -> [[x]] 
uniqueDays result = unique . properDays . listOfDays $ result

-- Changes String to Double
toDouble :: String -> Double
toDouble = read

-- Gets Average of List of Doubles
average :: [Double] -> Double
average xs = round1dp (sum xs / (int2Double $ length xs))

-- Apply a function on a list
apply :: (t -> a) -> [t] -> [a]
apply f xs = [f x | x <- xs]

round1dp :: Double -> Double
round1dp x = fromIntegral (round $ x * 1e1) / 1e1