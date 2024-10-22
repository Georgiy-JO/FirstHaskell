{-# LANGUAGE ScopedTypeVariables #-}
import Data.List
import Data.Char
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import Text.Read

main :: IO ()  --main function
main = do
    hSetBuffering stdout NoBuffering
    putStr "\t\t\tWellcome to the program!\n"
    putStr "\tHere are some commands:\n"
    putStr "  Tofile - puts data in a file\n  Fromfile - prints all data from the file\n  pers1 ,.. pers8 - persons data (1-4 - original, 5-8 - one that got from file)\n"
    putStr "  tpers1 ,.. tpers8 - same, but the way program see it\n "
    putStr "\tAttention! There is no error check for using 'Fromfile' or any of 'pers5',..'pers8' before 'Tofile'!\n"
    putStr "Input the number of commands you need-->"
    --let i=100
    i  <- readLn :: IO Int 
    forM_ [1..(i+1)] $ \j -> do
        putStr "Request #"
        putStr (show j)
        if(j>i) 
            then putStrLn "><Enter any key><"
            else putStr " Ask me to do-->"
        comd <- getLine 
        if (comd=="Tofile") then wrrite1
                else if (comd== "Fromfile") then readandprint
                        else if (comd== "tpers1") then putStrLn (show p1)
                                else if (comd== "tpers2") then putStrLn (show p2)
                                        else if (comd== "tpers3") then putStrLn (show p3)
                                                else if (comd== "tpers4") then putStrLn (show p4)
                                                        else if (comd== "tpers5")  then putStrLn (show (imveryunsafe p5))
                                                                else if (comd== "tpers6") then putStrLn (show (imveryunsafe p6))
                                                                        else if (comd== "tpers7")  then putStrLn (show (imveryunsafe p7))
                                                                                else if (comd== "tpers8") then putStrLn (show (imveryunsafe p8))
                                                                                        else if (comd== "pers1") then putStrLn (name p1 ++ " -> " ++ show (age p1) ++ "; " ++ show (weight p1) ++ " .")
                                                                                                else if (comd== "pers2") then putStrLn (name p2 ++ " -> " ++ show (age p2) ++ "; " ++ show (weight p2) ++ " .")
                                                                                                    else if (comd== "pers3") then putStrLn (name2 p3 ++ " -> " ++ show (age2 p3) ++ "; " ++ show (weight2 p3) ++ " .")
                                                                                                                else if (comd== "pers4") then putStrLn (name2 p4 ++ " -> " ++ show (age2 p4) ++ "; " ++ show (weight2 p4) ++ " .")
                                                                                                                        else if (comd== "pers5") then putStrLn (name2 (imveryunsafe  p5) ++ " -> " ++ show (age2 (imveryunsafe  p5)) ++ "; " ++ show (weight2 (imveryunsafe  p5)) ++ " .")
                                                                                                                                else if (comd== "pers6") then putStrLn (name2 (imveryunsafe  p6) ++ " -> " ++ show (age2 (imveryunsafe  p6)) ++ "; " ++ show (weight2 (imveryunsafe  p6)) ++ " .")
                                                                                                                                        else if (comd== "pers7") then putStrLn (name (imveryunsafe  p7) ++ " -> " ++ show (age (imveryunsafe  p7)) ++ "; " ++ show (weight (imveryunsafe  p7)) ++ " .")
                                                                                                                                                else if (comd== "pers8") then putStrLn (name (imveryunsafe  p8) ++ " -> " ++ show (age (imveryunsafe  p8)) ++ "; " ++ show (weight (imveryunsafe  p8)) ++ " .")
                                                                                                                                                        else putStrLn "Error intput" 
    --return ()
   

imveryunsafe :: IO a -> a    --that one is unsafe but needed
imveryunsafe a = unsafePerformIO (a)

{-The first way to create person's type-}
data Person = Person String Int Double 
        deriving (Eq, Show, Read)
name :: Person -> String
name (Person name _ _) = name
age :: Person -> Int
age (Person _ age _) = age
weight :: Person -> Double
weight (Person _ _ waight) = waight

{-"p1" and "p2" are initialised the first way-}
p1 :: Person
p1 = Person "Jack" 42 72.5
p2 :: Person
p2=Person "Jim" 19 49.3

{-The second way to create person's type-}
data Person2 = Pr2 {
    name2 :: String,
    age2 :: Int,
    weight2 :: Double
} deriving (Eq, Show, Read)

{-"p3" and "p4" are initialised the second way-}
p3 :: Person2
p3=Pr2 {
    name2 = "Kim",
    age2 = 25,
    weight2 = 45.4
}
p4 :: Person2
p4=Pr2 {
    name2 = "Sasha",
    age2 = 20,
    weight2 = 43.5
}


wrrite1 :: IO () --The first way to write into the file
wrrite1 = do
    writeFile "output.txt" (printer p1 ++"\n"++ printer p2 ++"\n"++ printer2 p3 ++"\n"++ printer2 p4)

{-functions that are needed to make "wrrite1" look easier -}
printer :: Person -> [Char]
printer x = name x ++ ": " ++ show (age x) ++", "++ show (weight x)++";"
printer2 :: Person2 -> [Char]
printer2 x = name2 x ++ ": "++ show (age2 x) ++", "++ show (weight2 x)++";"
 
wrrite2 :: IO()   --The second (simple) way to write into the file
wrrite2 = do
    writeFile "output2.txt" (show p1 ++"\n"++ show p2++"\n"++ show p3++"\n"++ show p4)
{-There will be NO read for wrrite2. I can create if it is needed -}

{-The function that takes information from the file for the first kind of person's type-}
{-Further there are functions that are needed for "read1" and "read2" to operate-}
--read1 :: IO (String)
read1 :: Int->IO (Person)
read1 i = do
    read_file <- readFile "output.txt"
    let lines_list = lines read_file
    let list = map namesf lines_list
    let a = list!!(i-1)
    let list2 = map agef lines_list
    let temp=  list2!!(i-1)
    let b = read (temp) :: Int
    let list3 = map weightf lines_list
    let c = read (list3!!(i-1)) ::Double 
    let res =  Person a b c
    return (res)

{-The function that takes information from the file for the second kind of person's type-}
read2 :: Int->IO (Person2)
read2 i = do
    read_file <- readFile "output.txt"
    let lines_list = lines read_file
    let list = map namesf lines_list
    let a = list!!(i-1)
    let list2 = map agef lines_list
    let temp=  list2!!(i-1)
    let b = read (temp) :: Int
    let list3 = map weightf lines_list
    let c = read (list3!!(i-1)) ::Double 
    let res =  Pr2 {
        name2 = a,
        age2 = b,
        weight2 =c}
    return (res)

{-The function that takes all the information from the file and prints it-}
readandprint = do
    read_file <- readFile "output.txt"
    putStrLn read_file 

namesf str = foldl (\(ys) x -> if ((x<='z') && (x>='a') || (x<='Z') && (x>='A')) then (ys) ++ [x]  else (ys)) [] str --function that returns a name from a line that was created from any persons's type
numsf str = foldl (\(ys) x -> if ((x<='9') && (x>='0') || (x==' ') || (x=='.')) then (ys) ++ [x]  else (ys)) [] str  -- function that returns only numbers from a line (in string type) 
{-next 2 functions are ment to return only age from a String created by "numsf"-}
agef2 (x:xs) = if(x/=' ')
                then [x] ++ agef2 (xs)
                else []
agef str = agef2 (tail(numsf str))
{-next 4 are asking "read1 and "read2" to return particular personalities-}
p5 :: IO Person2
p5=read2 1
p6=read2 2
p7=read1 3
p8=read1 4

{-next 2 functions are ment to return only weight from a String created by "numsf"-}
weightf1 (x:xs) = if (x==' ')
                     then xs
                     else weightf1 (xs) 
weightf str =  weightf1 (tail(numsf str))
