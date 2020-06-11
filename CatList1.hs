module CatList1 (CatList1,isEmpty1,cat1,cons1,snoc1,head1,tail1,newCat1) where
import Prelude hiding (head1, tail1)
import Queue1    


data CatList1 val = Empty | Cat val (Queue1 (CatList1 val)) deriving (Show, Eq)

newCat1 :: Eq val => [val] -> CatList1 val 
newCat1 [] = Empty
newCat1 (a:list) = if (newCat1 list == Empty) then  Cat a (createQ1 [] []) else Cat a (createQ1 [(newCat1 list)] [])

isEmpty1 :: CatList1 val -> Bool
isEmpty1 Empty = True
isEmpty1 (Cat val q) = False

link :: CatList1 val ->CatList1 val-> CatList1 val
link (Cat x queue) s= Cat x (snocQ1 queue s)

cat1 :: CatList1 val -> CatList1 val ->CatList1 val
cat1 Empty Empty = Empty
cat1 Empty x = x
cat1 x Empty = x
cat1 x y = (link x y)

cons1 :: val -> CatList1 val -> CatList1 val
cons1 x catList = cat1 (Cat x emptyQ1) catList

snoc1 :: CatList1 val -> val -> CatList1 val
snoc1 catList x = cat1 catList (Cat x emptyQ1)

head1 :: CatList1 val -> Maybe val
head1 Empty = Nothing
head1 (Cat x q) = Just x


linkAll :: Queue1 (CatList1 val) -> CatList1 val
linkAll queue = let queueHead = headQ1 queue 
                    queueTail = tailQ1 queue
                in if (isEmptyQ1 queueTail)
                   then  queueHead 
                   else (link  queueHead (linkAll queueTail))     

tail1 :: CatList1 val -> CatList1 val
tail1 Empty = Empty
tail1 (Cat x q) = if (isEmptyQ1 q) then Empty else (linkAll q) 


