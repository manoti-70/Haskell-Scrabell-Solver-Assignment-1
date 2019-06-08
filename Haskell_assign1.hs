--1.Find the last but one element of a list.
myLast xs = last xs

--2.Reverse a list.
myReverse xs = reverse xs

--3.Find out whether a list is a palindrome.
isPalindrome xs = [if reverse xs == xs then True else False]

--5.Duplicate the elements of a list.
dupli [] = []
dupli(x:xs) = take 2 (repeat x) ++ dupli xs

--7.Insert an element at a given position into a list.
insertAt x xs z = take (z-1) xs ++ [x] ++ drop (z-1) xs 

--6.Rotate a list N places to the left.
rotate xs (n) = [if (n>=0) then( drop (n) xs ++ take (n) xs) else drop (length xs + 2 ) xs ++ take (length xs + 2 ) xs]
--not possible for negative ,why?

--9.Determine whether a given integer number is prime.
isPrime x = if (length [x | xs <-[2..x-1] ,x `mod` xs ==0 ]) >0 then False else True

--4.Eliminate consecutive duplicates of list elements.
compress a
    | (xs == []) = [x]
    | (x == head xs) = [] ++ compress xs
    | otherwise = [x] ++ compress xs
    where x = head a
          xs = tail a

--8.Generate the combinations of K distinct objects chosen from the N
--elements of a list.
combinations 0 _ = [[]]
combinations n xs = [xs !! index : x | index <- [0..(length xs)-1] , x <- combinations (n - 1) (drop (index + 1) xs) ]

