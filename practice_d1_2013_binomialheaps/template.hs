type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

-- key :: BinTree a -> a
-- key
--   = undefined

value :: BinTree a -> a
value (Node v _ _)
  = v

rank :: BinTree a -> Int
rank (Node _ r _)
  = r

children :: BinTree a -> [BinTree a]
children (Node _ _ c)
  = c

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees t t'
  | value t < value t' = Node (value t) (1 + rank t) (t' : children t)
  | otherwise          = Node (value t') (1 + rank t') (t : children t')

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
-- Pre: heap is non-empty
extractMin (t : ts)
  = extractMin' ts (value t)
  where
    extractMin' :: Ord a => BinHeap a -> a -> a
    extractMin' [] min
      = min
    extractMin' (t : ts) min
      | (value t) < min = extractMin' ts (value t)
      | otherwise       = extractMin' ts min

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps h [] = h
mergeHeaps [] h = h
mergeHeaps h@(t : ts) h'@(t' : ts')
  | rank t  < rank t' = t  : (mergeHeaps ts h')
  | rank t' < rank t  = t' : (mergeHeaps ts' h)
  | otherwise         = (mergeHeaps [combineTrees t t'] (mergeHeaps ts ts'))

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert v h
  = mergeHeaps [Node v 0 []] h

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin h
  = mergeHeaps h' ((snd . removeMin) h)
  where
    h' = (reverse . children . fst . removeMin) h

remove :: Eq a => a -> BinHeap a -> BinHeap a
remove
  = undefined

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin h
  = removeMin' h []
  where
    m = extractMin h
    removeMin' (t:ts) h'
      | m == value t = (t, h' ++ ts)
      | otherwise    = removeMin' ts (t : h')

binSort :: Ord a => [a] -> [a]
binSort lst
  = binSort'' heap
  where
    heap = binSort' lst
binSort' (c : [])
  = insert c []
binSort' (c : cs)
  = insert c (binSort' cs)
binSort'' []
  = []
binSort'' h
  = (extractMin h) : binSort'' (deleteMin h)

--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary h
  = reverse (toBinary' h 0)
  where
    toBinary' [] i
      = []
    toBinary' (t : ts) i
      | i == rank t     = 1 : toBinary' ts (i + 1)
      | i < rank t      = 0 : toBinary' (t : ts) (i + 1)
      | i > rank t      = toBinary' ts i

binarySum :: [Int] -> [Int] -> [Int]
binarySum is js
  = reverse (binarySum' (reverse is) (reverse js) 0)

binarySum' :: [Int] -> [Int] -> Int -> [Int]
binarySum' [] [] c
  | c == 1 = [1]
  | otherwise = []
binarySum' (i : is) [] c
  = sum : binarySum' is [] carry
  where
    (sum, carry) = fullAdder i 0 c
binarySum' [] js c
  = binarySum' js [] c
binarySum' (i : is) (j : js) c
  = sum : binarySum' is js carry
  where
    (sum, carry) = fullAdder i j c

fullAdder :: Int -> Int -> Int -> (Int, Int)
fullAdder b1 b2 c
  = (sum, carry)
  where
    sum = (b1 + b2 + c) `mod` 2
    carry = (b1 + b2 + c) `div` 2 

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]



