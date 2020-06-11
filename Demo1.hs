import System.IO
import System.Environment
import CatList1
import Prelude hiding (head, tail)
import Control.Exception
import Data.Time


listGenerator :: Int -> [Int]
listGenerator n = if (n>0) then n : (listGenerator (n - 1) ) else [0]

l=newCat1 (listGenerator 50000)
k=newCat1 (listGenerator 50000)

main :: IO()
main=do
    output <- openFile "output.txt" WriteMode
    start <- getCurrentTime
    hPutStrLn output (show (cat1 l k))
    hPutStrLn output (show (snoc1 (cons1 0 (cat1 l k)) 5))
    hPutStrLn output (show (head1 (snoc1 (cons1 0 (cat1 l k)) 5)))
    hPutStrLn output (show (tail1 (snoc1 (cons1 0 (cat1 l k)) 5)))
    end <- getCurrentTime
    hClose output
    print (diffUTCTime end start)

-------Doua liste de 50000 nr
--669.6162912s
-------Doua liste de 5000 nr
--1.5262059s
-------Doua liste de 1000 nr 
--0.0573649s