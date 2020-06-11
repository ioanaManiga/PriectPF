module Queue (Queue,headQ, tailQ, snocQ, createQ, emptyQ, isEmptyQ) where
import SizeList

data Queue a = MyQ (SizeList a) (SizeList a) deriving(Show, Eq)

emptyQ :: Queue a
emptyQ = MyQ empty empty

isEmptyQ :: Queue a -> Bool
isEmptyQ (MyQ x y) = if (isEmptyL x)&&(isEmptyL y) then True else False

checkQ :: Queue a -> Queue a
checkQ (MyQ x y) =
     if (size x) == 0 
     then (MyQ (concatR x y) (empty))    
     else (MyQ x y)

headQ :: Queue a -> a 
headQ (MyQ  x  y) = (headL x) 
 
tailQ :: Queue a -> Queue a
tailQ (MyQ x y) = (checkQ (MyQ (tailL x) y))

snocQ :: Queue a -> a -> Queue a
snocQ (MyQ x y) n = (checkQ (MyQ x (addOne y n)))

createQ :: [a]->[a]->Queue a
createQ [] [] = (checkQ (MyQ empty empty)) 
createQ [] x = (checkQ (MyQ empty (newList x)))
createQ x [] = (checkQ (MyQ (newList x) empty))
createQ x y = (checkQ (MyQ (newList x) (newList y)))





