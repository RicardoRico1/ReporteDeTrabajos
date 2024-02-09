module FizzBuzz where   
fizz :: Int -> String
fizz n
    | n `mod` 3 == 0 && n `mod` 5 /= 0  ="Fizz!"
    | n `mod` 5 == 0 && n `mod` 3 /= 0 = "Buzz!"
    | n `mod` 5 == 0 && n `mod` 3 == 0 = "FizzBuzz!"
    | n >0 && n< 101 = number n

lessThan20 :: Int -> String
lessThan20 n
   |n >1 && n<20 =
   let answers = words ("one two three four five six seven eigth nine ten " ++
                                   "eleven twelve thirteen fourteen fifteen sixteen "++
                                       "seventeen eigtheen nineteen")
   in answers!!(n-1)
tens :: Int -> String
tens n 
    | n > 1 && n <= 9 =
        answers!!(n-2)
        where
            answers = words "twenty thirty forty fivety sixty seventy eighty ninety"
--
number :: Int -> String
number n
    | n>0 && n<20 = lessThan20 n
    | n `mod` 10 == 0 && n < 100 = tens (n `div` 10)
    | n < 100 = tens (n `div` 10) ++ " " ++ lessThan20(n `mod` 10)    
       