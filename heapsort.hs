import System.Random (randomRIO)

randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,100000)
  rs <- randomList (n-1)
  return (r:rs)

data Heap a = Nil | Node a Int Int (Heap a) (Heap a) deriving (Show, Eq)

node (Node e _ _ _ _) = e

dept Nil = 0
dept (Node _ _ d _ _) = d

partial Nil = True
partial (Node _ n d _ _) = n < 2 ^ d - 1

heapify x@(Node _ _ _ Nil Nil) = x
heapify x@(Node e n d l r)
  | e > node l && (r == Nil || node l < node r) = Node (node l) n d l' r
  | r /= Nil && e > node r = Node (node r) n d l r'
  | otherwise = x
  where
  l' = update e l
  r' = update e r
  update x Nil = Node x 1 1 Nil Nil
  update x (Node _ n d l r) = heapify (Node x n d l r)

insert x Nil = Node x 1 1 Nil Nil
insert x (Node e n d l r)
  | partial l = heapify (Node e (n + 1) (dept l' + 1) l' r)
  | partial r || dept r < dept l = heapify (Node e (n + 1) (dept l + 1) l r')
  | otherwise = heapify (Node e (n + 1) (dept l' + 1) l' r)
  where
  l' = insert x l
  r' = insert x r

delete (Node _ _ _ Nil Nil) = Nil
delete (Node _ n _ l r)
  | partial l = heapify (Node (node l) (n - 1) (dept l' + 1) l' r)
  | dept l == dept r = heapify (Node (node r) (n - 1) (dept l + 1) l r')
  | otherwise = heapify (Node (node l) (n - 1) (dept l' + 1) l' r)
  where
  l' = delete l
  r' = delete r

makeheap [] = Nil
makeheap x = foldl (flip insert) Nil x

flatheap Nil = []
flatheap x = (node x) : flatheap (delete x)

heapsort x = flatheap (makeheap x)

ascending [x] = True
ascending [x,y] = x <= y
ascending (x:y:l) = x <= y && ascending (y:l)

