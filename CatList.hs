module CatList (CatList,isEmpty,cat,cons,snoc,head,tail,newCat) where
import Prelude hiding (head, tail)
import Queue    

data CatList val = Empty | Cat val (Queue (CatList val)) deriving (Show, Eq)


newCat :: Eq val => [val] -> CatList val 
newCat [] = Empty
newCat (a:list) = if (newCat list == Empty) then  Cat a (createQ [] []) else Cat a (createQ [(newCat list)] [])

isEmpty :: CatList val -> Bool
isEmpty Empty = True
isEmpty (Cat val q) = False

link :: CatList val ->CatList val-> CatList val
link (Cat x queue) s= Cat x (snocQ queue s)

cat :: CatList val -> CatList val ->CatList val
cat Empty Empty = Empty
cat Empty x = x
cat x Empty = x
cat x y = (link x y)

cons :: val -> CatList val -> CatList val
cons x catList = cat (Cat x emptyQ) catList

snoc :: CatList val -> val -> CatList val
snoc catList x = cat catList (Cat x emptyQ)

head :: CatList val -> Maybe val
head Empty = Nothing
head (Cat x q) = Just x


linkAll :: Queue (CatList val) -> CatList val
linkAll queue = let queueHead = headQ queue 
                    queueTail = tailQ queue
                in if (isEmptyQ queueTail)
                   then  queueHead 
                   else (link  queueHead (linkAll queueTail))     

tail :: CatList val -> CatList val
tail Empty = Empty
tail (Cat x q) = if (isEmptyQ q) then Empty else (linkAll q) 


