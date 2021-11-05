--PART 1A--
{-Firstly, the use of functional programming has the significant benefit of writing more succinct programs. Features such as
higher-order functions (e.g. map), and polymorphic functions(e.g. length) allow functions to be written while omitting
certain intermediate steps that may be required within other programming paradigms. 
Secondly, the speed and efficiency of functional programming allows programs to execute faster, with less impact on
CPU performance. This is because of lazy comprehension, where functions are not executed until their result is required 
within the program. As a consequence, CPU processing is only used when required and therefore is never wasted.
Thirdly, functional programming allows functions of a generic type to be defined, known as polymprphic functions. This means
programmers can reuse a given polymorphic function with data of any type. Thus, they can save time when constructing programs and
do not have to define multiple functions with an identical purpose, as well as defining the constraints required by the input
data to run them.-} 

--PART 1B--
{-A mathematical function takes a given input and maps it to a given output based on defined criteria and operations. The given
criteria may be the type of the input and output, as well as the number of arguments or parameters required. The operations
that are performed will specify hoe the data is manipulated in order to produce the output. In Haskell, functions are similar to 
mathematical functions as they operate as pure functions, taking a given input of one type and mapping it to an output of another 
type, regardless of any external factors or variables.-}

--PART 1C--
{-A higher-order function can be defined as a function that maps a given function to another, taking one as the argument and 
returning another as the output. For example, the higher-order library function "map" is used to apply a given function to 
every element of a list; the function may specify the addition of an integer value, for instance, which can then be applied 
to every element in a list of integers and returned as a new list containing the results. Another example of a higher-order 
function is filter, which is used to return elements from a list that satisfy a given predicate; this may be used to return 
all values from a list of integers that are even, for example.-}

--(HUTTON, 2016. Programming in Haskell. 2nd edition. Cambridge University Press.)--

--PART 2A--
--First define a function to print one rectangle of given width and height
rectangle :: Int -> Int -> [[Char]]
rectangle m x = replicate x(replicate m '*')

--create a list of rectangles of increasing width by a scale factor of p
--This only creates the list of ascending steps, reversing this will give the descending stairs
stepUp :: Int -> Int -> Int -> [[Char]]
stepUp m n p = concat([rectangle (m*i) n | i <- [1..p]])

steps :: Int -> Int -> Int -> [Char]
steps m n p = unlines(stepUp m n p)++unlines(reverse(stepUp m n p))

--PART 2B--
--Functions to track even and odd numbers - used in later functions
evenCountFrom5 :: Int->Int
evenCountFrom5 x = length(filter even[5..x])

oddCountFrom6 :: Int->Int
oddCountFrom6 x = length(filter odd[6..x])

oddCountFrom5 :: Int->Int
oddCountFrom5 x = length(filter odd[5..x])

--line creation functions
--first line used in each flag, replicates '*' n times
line :: Int->[Char]
line n = replicate n '*'
--line used for the 4 character flag lines
lineB :: Int -> Int -> [Char]
lineB n i = replicate 1 '*' ++ replicate i ' ' ++ replicate 1 '*'++ replicate (n-4-(2*i)) ' ' ++ replicate 1 '*' ++ replicate i ' ' ++ replicate 1 '*'
--line used for 3 character symbols lines within odd numbered flags
lineC :: Int ->[Char]
lineC n = replicate 1 '*' ++ replicate (oddCountFrom5 n) ' ' ++ replicate 1 '*' ++ replicate (oddCountFrom5 n) ' ' ++ replicate 1 '*'
--even flag creation, combining lines based on the number of even numbers from 5
--creates a list of strings that resemble the first half of the flag when printed line by line
flagpatternE :: Int -> [[Char]]
flagpatternE x = [line x]++[lineB x i | i <- [0..(evenCountFrom5 x)]]
--odd flag creation, combining lines based on the number of odd numbers from 6
--creates a list of strings that resemble the first half of the flag when printed line by line
flagpatternO :: Int -> [[Char]]
flagpatternO x = [line x]++[lineB x i | i<-[0..(oddCountFrom6 x)]]

--flag combination function
--check for odd or even flag and create correspinding list of strings accordingly
--using the defined flag functions, reverse for the seconf half of the flag
--the odd flag requires a seperate middle line before the reverse is added
flagCreate :: Int -> [Char]
flagCreate x
 | x `mod` 2 == 0 = unlines(flagpatternE x) ++ unlines(reverse(flagpatternE x))
 |otherwise = unlines(flagpatternO x) ++ lineC x ++ ['\n']++ unlines(reverse(flagpatternO x))
 
--main flagpattern function
--creates the flag based on the size entered
--prints the number of flags based on the second number entered
flagpattern :: Int->Int->[Char]
flagpattern n x = concat([flagCreate n | x<-[1..x]])

--PART 3--
--function to remove duplicates from two given lists, 
--returning a string of unique characters
removeDup :: [Char]->[Char]->[Char]
removeDup xs ys = filter (\x -> not (x `elem` ys)) xs
--calculate the place in the list for lphi
--i.e. there are 4 options therefore mod 4 must be used
calcmod4 :: [Char] -> [Char] -> Int
calcmod4 x y = (length (removeDup x y)) `mod` 4
--assign string to result of modulus
lphi :: Int -> [Char]
lphi x
 |x==1 = " loves "
 |x==2 = " is physical to "
 |x==3 = " hates "
 |otherwise = " is indifferent to "
--main compatibility function, combines the previous functions as one string
compatibility  :: [Char] -> [Char] -> [Char]
compatibility  xs ys = xs ++ lphi (calcmod4 xs ys) ++ ys ++ " and " ++ ys ++ lphi(calcmod4 ys xs) ++ xs

--PART 4--
{-split the list at the given index variable using break, a tuple is returned containing a pair of lists
The first element is the first list of elements that do not satisfy the preciate
The second list is the remiaineder of the initial list (the second element in the tuple)
The split function is therefore repeated on the rest of the list, adding the elements to a new list each time the split
element is reached, this element is also removed from the remainder lists by dropping the first element. -}
isplit :: Eq a => [a] -> a -> [[a]]
isplit [] i = []
isplit ys i = fst(break(==i)ys) : isplit (drop 1 (snd(break(==i)ys))) i
--map length to each sublist in order to output the number of elements in each split
lsplit :: Eq a => [a] -> a -> [Int]
lsplit xs i = map (length) (isplit xs i)