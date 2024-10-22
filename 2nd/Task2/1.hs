import Control.Monad
import Data.List
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Barans

main :: IO ()
main = do                                                       --askes THE NUMBER of the sheep user is interested in and returnes all the information that is asked in the task
    putStrLn "\t\t\tWellcome to the program!"
    putStrLn "\tInput a number of the sheep you are interested in: "                    --ONLY NUMBER (NOT THE NAME)  <== NAME = "i"NUMBER = "i" ++ NUMBER
    sheeppy' <- getLine
    let sheeppy = "i" ++ sheeppy'
    putStr "The grandfather of the sheep (mother's side): "
    let mgrandf= grandfather sheeppy
    if (mgrandf == Nothing)
        then putStrLn ("No information")
        else putStrLn (show (untyjust (mgrandf)) ++ "   Number ->" ++ getnum(untyjust (mgrandf)))
    if (mgrandf /= Nothing)
        then if (Barans.father(untyjust(mgrandf)) == Nothing)
                 then putStrLn ("No information about its greatgrandfather")
                 else putStrLn ("Its greatgrandfather's name is " ++ show (untyjust (Barans.father(untyjust(mgrandf)))) ++ "   Number ->" ++ getnum(untyjust (Barans.father(untyjust(mgrandf)))))
        else putStrLn ("")
    let parlist= parents sheeppy (Barans.father sheeppy)
    if (parlist==[])
        then  putStrLn ("No information about parents of the sheep. The sheep is an orphan(according to the data base).")
        else putStrLn ("Information about the sheep's parents (names): " ++ show (parlist) ++ " -> It is not an orphan (according to the data base).")
    let grandparlist = grandparents ( parents sheeppy (Barans.father sheeppy))
    if (grandparlist==[])
        then  putStrLn ("No information about grandparents of the sheep")
        else putStrLn ("Information about the sheep's grandparents (names): " ++ show (grandparlist))
    putStrLn ("System information about selected father of the sheep:" ++ show (checkfather sheeppy selected_barans))
    putStrLn ("System information about closest selected male father line ancestor of the sheep:" ++ show (checkfather' sheeppy selected_barans))
    putStrLn "Pause.Press Enter"
    sheeppy' <- getLine                                                    --the simplest way to stop the program from closing 
    putStrLn "THE END"

 
    --return
 

untyjust :: Maybe a -> a
untyjust (Just s) = s                                                       --my way to get rid of JUST
getnum :: [a] -> [a]
getnum (x:xs) = (xs)                                                        --transform name of a sheep to its number
grandfather :: Sheep -> Maybe Sheep  
grandfather s | Barans.mother s == Nothing = Nothing                       --granddad finding function
              | otherwise = Barans.father (untyjust(Barans.mother s))

parents :: Sheep -> Maybe Sheep -> [[Char]]                                --parents to get rarents list
parents s Nothing | Barans.mother s == Nothing = []
                  |otherwise = [untyjust (Barans.mother s)]
parents s (Just f) = [f] ++ parents s Nothing

grandparents :: [Sheep] -> [[Char]]
grandparents [] = []
grandparents [x] = parents x (Barans.father x)
grandparents [x,y]= parents x (Barans.father x) ++ parents y (Barans.father y)   --returnes list of grandparents (could be done more beautiful if "parents" would not return a complite list but just names)

selected_barans :: [[Char]]
selected_barans = ["i3", "i5", "i6", "i9", "i12"]                                     --The list of the selected ones! for test2: ["i3", "i5", "i9", "i12"] 


checkfather :: Sheep -> [Sheep] -> Maybe Sheep
checkfather y [] = Nothing
checkfather y (x:xs) | Barans.father y == Nothing = Nothing
                     | x == untyjust (Barans.father y) = Barans.father y               --is checking using Just-Nothing is the father of the sheep is selected
                     | otherwise = checkfather y (xs)

checkfather' :: Sheep -> [Sheep] -> Maybe Sheep
checkfather' y [] | Barans.father y /= Nothing = checkfather' (untyjust (Barans.father y)) selected_barans   --checking using Just-Nothing is any male line ancestor of the sheep is selected
                  | otherwise = Nothing
checkfather' y (x:xs) | Barans.father y == Nothing = Nothing
                      |x == untyjust (Barans.father y) = Barans.father y
                      | otherwise = checkfather' y (xs)