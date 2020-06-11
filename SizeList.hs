module SizeList (empty,isEmptyL, headL, tailL, getSize,newList ,size,concatR, list, addOne, SizeList) where

data SizeList a = Slist (Int) [a] deriving (Show, Eq)

getSize  :: [a] -> Int
getSize []     = 0
getSize (_:x) = 1 + (getSize x)

empty :: SizeList a
empty = Slist 0 []

isEmptyL :: SizeList a -> Bool
isEmptyL (Slist 0 []) =True
isEmptyL _ = False

headL :: SizeList a -> a
headL  (Slist n (x:_)) = x

tailL :: SizeList a -> SizeList a
tailL (Slist 0 []) = Slist 0 []
tailL (Slist n (_:list)) = Slist (n-1) list

addOne :: SizeList a -> a -> SizeList a
addOne (Slist x list) a = Slist (x+1) (a:list) 

addAll :: SizeList a -> [a] -> SizeList a
addAll (Slist x list) l = Slist (x+(getSize l)) (l++list) 

newList :: [a] -> SizeList a
newList list = addAll empty list

size :: SizeList a -> Int
size (Slist x list) = x

list :: SizeList a -> [a]
list (Slist x list) = list

concatR :: SizeList a -> SizeList a -> SizeList a
concatR (Slist n list1) (Slist m list2) = (Slist (n+m) (list1++(reverse list2)))


