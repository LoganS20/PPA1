-- 159272 Assignment 1
-- Logan Sim 21014553

    -- The top-level function --

adjust_date :: (Int, Int, Int) -> Int -> (Int, Int, Int)
adjust_date (d, m, y) x
    | checkDate (d, m, y)  = error "The date must be a valid date between (1, 1, 1600) and (31, 12, 3000)" 
    | checkOffset x        = error "The offset must be a valid number between -25 and 25"
    | otherwise            = shiftDate (d, m, y) x


    --- Misc Functions ---

-- Determines if it is a is leapyear
    -- Tip: a leap year is a year that is an integer multiple of 4 (except for years evenly divisible by 100, but not by 400).
leapYear :: Int -> Bool
leapYear y
    | (mod y 100 == 0) && (mod y 400 > 0)  = False
    | mod y 4 > 0                          = False          
    | otherwise                            = True
    
-- Checks how many days are in a month
monthLen :: (Int, Int) -> Int
monthLen (m, y)
    | m == 1               = 31    -- Jan
    | m == 2 && leapYear y = 29    -- Feb in a leap year
    | m == 2               = 28    -- Feb
    | m == 3               = 31    -- Mar
    | m == 4               = 30    -- Apr
    | m == 5               = 31    -- May
    | m == 6               = 30    -- Jun
    | m == 7               = 31    -- Jul
    | m == 8               = 31    -- Aug
    | m == 9               = 30    -- Sep
    | m == 10              = 31    -- Oct
    | m == 11              = 30    -- Nov
    | m == 12              = 31    -- Dec
    | otherwise            = error "Error: Month not valid"
    

    -- Input Error Related Functions --

-- Check if date is within the acceptable parameters (1, 1, 1600) - (31, 12, 3000)
checkDate :: (Int, Int, Int) -> Bool
checkDate (d, m, y)
    | checkDay (d, m, y) && (m >= 1 && m <= 12) && (y >= 1600 && y <= 3000) = False
    | otherwise                                                             = True

-- Checks that the day is a valid day. 
    -- Cannot be more days than in a given month (Must account for leap year Feb)
checkDay :: (Int, Int, Int) -> Bool
checkDay (d, m, y)
    | d >= 1 && d <= monthLen (m, y) = True 
    | otherwise                     = False

-- Checks the offset is between 25 and -25
checkOffset :: Int -> Bool
checkOffset x
    | x >= (-25) && x <= 25 = False
    | otherwise             = True


    -- Function to Change the Date --

-- Return the final Date change
shiftDate :: (Int, Int, Int) -> Int -> (Int, Int, Int)
shiftDate (d, m, y) x
    | m < 1                   = shiftDate (monthLen (12, y-1), 12, y-1) x        -- Moving into previous year
    | (d+x) <= 0              = shiftDate (monthLen (m-1, y), m-1, y) (d+x)      -- Move into previous month

    | m > 12                  = shiftDate (0, 1, y+1) x                          -- Moving into next year
    | (d+x) > monthLen (m, y) = shiftDate (0, m+1, y) ((d+x) - monthLen (m, y))  -- Move into next month

    | otherwise               = (d+x, m, y)                                      -- Change the date








