module Queue1 (Queue1,headQ1, tailQ1, snocQ1, createQ1, emptyQ1, isEmptyQ1) where
import SizeList

data Queue1 a = MyQ (SizeList a) (SizeList a) deriving(Show, Eq)

emptyQ1 :: Queue1 a
emptyQ1 = MyQ empty empty

isEmptyQ1 :: Queue1 a -> Bool
isEmptyQ1 (MyQ x y) = if (isEmptyL x)&&(isEmptyL y) then True else False

checkQ :: Queue1 a -> Queue1 a
checkQ (MyQ x y) =
     if (size x) < (size y) 
     then (MyQ (concatR x y) (empty))    
     else (MyQ x y)

headQ1 :: Queue1 a -> a 
headQ1 (MyQ  x  y) = (headL x) 
 
tailQ1 :: Queue1 a -> Queue1 a
tailQ1 (MyQ x y) = (checkQ (MyQ (tailL x) y))

snocQ1 :: Queue1 a -> a -> Queue1 a
snocQ1 (MyQ x y) n = (checkQ (MyQ x (addOne y n)))

createQ1 :: [a]->[a]->Queue1 a
createQ1 [] [] = (checkQ (MyQ empty empty)) 
createQ1 [] x = (checkQ (MyQ empty (newList x)))
createQ1 x [] = (checkQ (MyQ (newList x) empty))
createQ1 x y = (checkQ (MyQ (newList x) (newList y)))




