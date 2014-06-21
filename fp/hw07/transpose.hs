
import Data.List

main = do 
    s <- readFile "input.txt"  
    writeFile "output.txt" . unlines . map unwords . transpose . map words . lines $ s 
