module Lib where

data Tree a = Leaf a Int | Node a Int (Tree a) (Tree a) --  the int value is designed for stroing size for the whole tree
     deriving Show

mapT f t = case t of (Leaf x 1) -> Leaf (f x) 1
                     (Node x s l r) -> Node (f x) s (mapT f l) (mapT f r)

instance Functor Tree where
  fmap f t= mapT f t

sizeT :: Tree a -> Int
sizeT (Leaf _ _) = 1
sizeT (Node x s l r) = s

data RAL a = RAL [Tree a] Int --  the int value is designed for stroing size for the whole list
     deriving Show

sizeRAL :: RAL a -> Int
sizeRAL (RAL _ s) = s



insert  :: a -> RAL a -> RAL a
insert x (RAL [] 0)= RAL [Leaf x 1] 1
insert x (RAL [t] s)= RAL ([Leaf x 1]++[t]) (1 + s)
insert x (RAL ts'@(t1:(t2:ts)) sr)| sizeT t1 == sizeT t2  = RAL ((Node x (1 + sizeT t1 + sizeT t2) t1 t2):ts) (1+sr)
                                  | otherwise = RAL ((Leaf x 1):ts') (1+sr)

remove :: RAL a -> (a,RAL a)
remove (RAL (t:ts) s) = case t of (Leaf x _) -> (x,RAL ts (s-1))
                                  (Node x _ l r) -> (x,RAL (l:(r:ts)) (s-1))

reverse' :: RAL a -> RAL a
reverse' ral = reverseHelper ral (RAL [] 0)

reverseHelper :: RAL a -> RAL a -> RAL a
reverseHelper (RAL [] 0) t = t
reverseHelper t1 t2 = reverseHelper (snd(remove t1)) (insert (fst(remove t1)) t2) 

insertListHelper  :: RAL a -> RAL a -> RAL a
insertListHelper (RAL [] 0) ral = ral
insertListHelper ral' ral = insertListHelper (ral'') (insert x ral) 
                      where (x,ral'') = remove ral'

insertList :: RAL a -> RAL a -> RAL a
insertList ral' ral = insertListHelper (reverse' ral') ral

instance Functor RAL where
  fmap f (RAL ts s) = RAL (map (mapT f) ts) s

instance Applicative RAL where
  pure = undefined
  (<*>) = undefined

instance Monad RAL where
  return x = RAL [Leaf x 1] 1
  (RAL [] 0) >>= _ = RAL [] 0
  r@(RAL (t:ts) s) >>= f = insertList (r' >>= f) (f x)
                          where (x,r') = remove r

e = RAL [] 0

f i k = if i==0 then k else insert i (f (i-1) k)
