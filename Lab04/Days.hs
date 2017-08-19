module Days where
    
-- | Time of Day
-- 
-- >>> getTimeOfDay Monday 10 AM
-- "Monday Morning"
-- 
-- >>> getTimeOfDay Wednesday 11 PM
-- "Wednesday Night"
--
-- >>> getTimeOfDay Sunday 12 PM
-- "Sunday Noon"
--
-- >>> getTimeOfDay Saturday 12AM
-- "Saturday Night"
--
-- >>> getTimeOfDay Monday 7 AM
-- "Monday Night"
--
-- >>> getTimeOfDay Tuesday 12 PM
-- "Tuesday Noon"
--
-- >>> getTimeOfDay Thursday 6 PM
-- "Thursday Evening"
--
-- >>> getTimeOfDay Friday 3 PM
-- "Friday Afternoon"
--
-- >>> getTimeOfDay Sunday 2 AM
-- "Sunday Night"

data Days = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    
data Convention = AM | PM
    
getTimeOfDay :: Days -> Integer -> Convention -> String
getTimeOfDay day y x = case day of
   Monday -> case x of
    AM
      | y <= 7 || y == 12 -> "Monday Night"
      | y <= 11 -> "Monday Morning"
    PM
      | y == 12 -> "Monday Noon"
      | y <= 5 -> "Monday Afternoon"
      | y <= 9 -> "Monday Evening"
      | y <= 12 -> "Monday Night"
   Tuesday -> case x of
    AM
      | y <= 7 || y == 12 -> "Tuesday Night"
      | y <= 11 -> "Tuesday Morning"
    PM
      | y == 12 -> "Tuesday Noon"
      | y <= 5 -> "Tuesday Afternoon"
      | y <= 9 -> "Tuesday Evening"
      | y <= 11 -> "Tuesday Night"
   Wednesday -> case x of
    AM
      | y <= 7 || y == 12 -> "Wednesday Night"
      | y <= 11 -> "Wednesday Morning"
    PM
      | y == 12 -> "Wednesday Noon"
      | y <= 5 -> "Wednesday Afternoon"
      | y <= 9 -> "Wednesday Evening"
      | y <= 11 -> "Wednesday Night"
   Thursday -> case x of
     AM
      | y <= 7 || y == 12 -> "Thursday Night"
      | y <= 11 -> "Thursday Morning"
     PM
      | y == 12 -> "Thursday Noon"
      | y <= 5 -> "Thursday Afternoon"
      | y <= 9 -> "Thursday Evening"
      | y <= 11 -> "Thursday Night"
   Friday -> case x of
    AM
      | y <= 7 || y == 12 -> "Friday Night"
      | y <= 11 -> "Friday Morning"
    PM
      | y == 12 -> "Friday Noon"
      | y <= 5 -> "Friday Afternoon"
      | y <= 9 -> "Friday Evening"
      | y <= 11 -> "Friday Night"
   Saturday -> case x of
    AM
      | y <= 7 || y == 12 -> "Saturday Night"
      | y <= 11 -> "Saturday Morning"
    PM
      | y == 12 -> "Saturday Noon"
      | y <= 5 -> "Saturday Afternoon"
      | y <= 9 -> "Saturday Evening"
      | y <= 11 -> "Saturday Night"
   Sunday -> case x of
    AM
      | y <= 7 || y == 12 -> "Sunday Night"
      | y <= 11 -> "Sunday Morning"
    PM
      | y == 12 -> "Sunday Noon"
      | y <= 5 -> "Sunday Afternoon"
      | y <= 9 -> "Sunday Evening"
      | y <= 11 -> "Sunday Night"
