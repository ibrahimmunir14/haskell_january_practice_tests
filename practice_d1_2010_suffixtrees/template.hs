data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix "" _ 
  = True
isPrefix _ ""
  = False
isPrefix (x:xs) (y:ys)
  | x == y    = isPrefix xs ys
  | otherwise = False

removePrefix :: String -> String -> String
--Pre: s is a prefix of s'
removePrefix "" s'
  = s'
removePrefix (a : s) (b : s')
  = removePrefix s s'

suffixes :: [a] -> [[a]]
suffixes []
  = []
suffixes (x : xs)
  = (x : xs) : suffixes xs

isSubstring :: String -> String -> Bool
isSubstring s t
  = or [isPrefix s suff | suff <- suffixes t]

findSubstrings :: String -> String -> [Int]
findSubstrings s t
  = [index | index <- [0..length(suffs)-1], isPrefix s (suffs!!index) == True]
  where
    suffs = suffixes t

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf x)
  = x : []
getIndices (Node subTreeList)
  = concat [getIndices st | (_, st) <- subTreeList]

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition [] ys
  = ([], [], ys)
partition xs []
  = ([], xs, [])
partition (x : xs) (y : ys)
  | x == y    = (x : a, b, c)
  | otherwise = ([], x : xs, y : ys)
  where
    (a, b, c) = partition xs ys

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' s (Node ((a, subTree) : subTreeList))
  | pre == s     = getIndices subTree
  | pre == a     = findSubstrings' (suf1) subTree
  | otherwise    = findSubstrings' s (Node subTreeList)
  where
    (pre, suf1, suf2) = partition s a
findSubstrings' _ _
  = []

-- findSubstrings' :: String -> SuffixTree -> [Int]
-- findSubstrings' s (Node ((a, subTree) : subTreeList))
--   | isPrefix s a = getIndices subTree
--   | isPrefix a s = findSubstrings' (removePrefix a s) subTree
--   | otherwise    = findSubstrings' s (Node subTreeList)

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s, n) (Node [])
    = Node [(s, Leaf n)]
insert (s, n) (Node ((a, t) : subTreeList))
  | p == [] && subTreeList == [] = Node ((a, t) : (s, Leaf n) : subTreeList)
  | p == [] = Node ((a, t) : subTreeList')
  | p == a  = Node ((a, insert (suf1, n) t) : subTreeList)
  | p /= a  = Node ((p, t') : subTreeList)
  where
    (p, suf1, suf2) = partition s a
    t' = Node [(suf1, (Leaf n)), (suf2, t)]
    (Node subTreeList') = insert (s, n) (Node subTreeList)


-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring 
  = undefined

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]


