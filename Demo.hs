import System.IO
import System.Environment
import CatList
import Prelude hiding (head, tail)
import Control.Exception
import Data.Time


listGenerator :: Int -> [Int]
listGenerator n = if (n>0) then n : (listGenerator (n - 1) ) else [0]

l=newCat (listGenerator 50000)
k=newCat (listGenerator 50000)

main :: IO()
main=do
    output <- openFile "output.txt" WriteMode
    start <- getCurrentTime
    hPutStrLn output (show (cat l k))
    hPutStrLn output (show (snoc (cons 0 (cat l k)) 5))
    hPutStrLn output (show (head (snoc (cons 0 (cat l k)) 5)))
    hPutStrLn output (show (tail (snoc (cons 0 (cat l k)) 5)))
    end <- getCurrentTime
    hClose output
    print (diffUTCTime end start)

-------Doua liste de 50000 nr
--681.2908112s
-------Doua liste de 5000 nr
--1.5213748s
-------Doua liste de 1000 nr 
--0.0572978s




