import System.Random 
import Data.List

help :: Int -> Char -> String -> String
help ind char word = (take ind word) ++ (char:"") ++ (drop (ind+1) word) 

replace :: Char -> [Int] -> String -> String -- recursive?
replace letter nums string = 
    if (length nums) ==0 then string
    else 
        replace letter (drop 1 nums) (help (nums !! 0) letter string) 

func :: String -> String -> IO()
func string orig = do
    if (elem '_' string) then 
        do 
            letter <- getLine 
            if (elem (letter !! 0) orig) && not (elem (letter !! 0) string) then do
                let occur = (elemIndices (letter !! 0) orig)
                putStrLn(replace (letter !! 0) occur string)
                func (replace (letter !! 0) occur string) orig
            else do 
		putStrLn(string)
                func string orig
    else putStrLn(string)

main :: IO ()
main = do
	file <- readFile "words.txt"
	let words = lines file
	let num = length words
	val <- randomRIO(0, num-1)
	let word = words !! val
	let len = length word 
	let start = take len (cycle ['_'])
	putStrLn(start)
	(func start word)




    
	

        
    

    