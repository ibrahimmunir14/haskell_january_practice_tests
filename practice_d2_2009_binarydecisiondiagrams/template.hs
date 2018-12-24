import Data.List

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp k' ((k, v) : kvs)
  | k' == k   = v
  | otherwise = lookUp k' kvs

checkSat :: BDD -> Env -> Bool
checkSat (rId, nodes) env
  = checkSat' (rId, (lookUp rId nodes))
  where
    checkSat' :: BDDNode -> Bool
    checkSat' (nId, (i, l, r))
      | lookUp i env
        = if (r == 0 || r == 1) then intToBool r else checkSat' (r, lookUp r nodes)
      | otherwise
        = if (l == 0 || l == 1) then intToBool l else checkSat' (l, lookUp l nodes)

intToBool :: Int -> Bool
intToBool 0 = False
intToBool 1 = True

sat :: BDD -> [[(Index, Bool)]]
sat (rId, [])
  = []
sat (rId, nodes)
  = sat' (rId, (lookUp rId nodes)) []
  where
    sat' :: BDDNode -> Env -> [[(Index, Bool)]]
    sat' (nId, (i, lId, rId)) env
      = sat'' rId True ++ sat'' lId False
      where
        sat'' lr b
          | lr == 1    = ((i, b) : env) : []
          | lr == 0    = []
          | otherwise  = sat' (lr, lookUp lr nodes) ((i, b) : env)

------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify (Not (Prim b))
  = Prim (not b)
simplify (Or (Prim b) (Prim b'))
  = Prim (b || b')
simplify (And (Prim b) (Prim b'))
  = Prim (b && b')
simplify exp
  = exp

restrict :: BExp -> Index -> Bool -> BExp
restrict e i b
  = restrict' e
  where
    restrict' (Prim x)
      = Prim x
    restrict' (IdRef j)
      | i == j    = Prim b
      | otherwise = IdRef j
    restrict' (Not exp)
      = simplify (Not (restrict' exp))
    restrict' (Or exp exp')
      = simplify (Or (restrict' exp) (restrict' exp'))
    restrict' (And exp exp')
      = simplify (And (restrict' exp) (restrict' exp'))

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD e xs
  = buildBDD' e 2 xs

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' (Prim False) nId []
  = (0, [])
buildBDD' (Prim True) nId []
  = (1, [])
buildBDD' e nId (x : xs)
  = (nId, [(nId, (x, leftId, rightId))] ++ leftNodes ++ rightNodes)
  where
    (leftId, leftNodes)   = buildBDD' (restrict e x False) (2*nId)     xs
    (rightId, rightNodes) = buildBDD' (restrict e x True)  (2*nId + 1) xs

------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD e xs
  = fst (buildROBDD' e 2 xs [])

buildROBDD' :: BExp -> NodeId -> [Index] -> [((Index,NodeId,NodeId), NodeId)] -> (BDD, [((Index,NodeId,NodeId), NodeId)])
buildROBDD' (Prim False) nId [] _
  = ((0, []), [])
buildROBDD' (Prim True) nId [] _
  = ((1, []), [])
buildROBDD' e nId (x : xs) hashtbl
  | leftId' == rightId'
    = buildROBDD' (restrict e x False) (2*nId) xs hashtbl
  | otherwise
    = ((nId, [(nId, (x, leftId', rightId'))] ++ leftNodes' ++ rightNodes'), ((x, leftId', rightId'), nId) : hashtblR)
  where
    leftPtr = if leftNodes == [] then -1 else findPtr (snd (leftNodes !! 0)) hashtbl
    rightPtr = if rightNodes == [] then -1 else findPtr (snd (rightNodes !! 0)) (hashtbl ++ hashtblL)
    ((leftId, leftNodes), hashtblL) = buildROBDD' (restrict e x False) (2*nId) xs hashtbl
    ((rightId, rightNodes), hashtblR) = buildROBDD' (restrict e x True) (2*nId + 1) xs (hashtblL ++ hashtbl)
    (leftId', leftNodes') = if (leftPtr == -1) then (leftId, leftNodes) else (leftPtr, [])
    (rightId', rightNodes') = if (rightPtr == -1) then (rightId, rightNodes) else (rightPtr, [])
findPtr :: (Index,NodeId,NodeId) -> [((Index,NodeId,NodeId), NodeId)] -> NodeId
findPtr _ []
  = -1
findPtr k' ((k, v) : kvs)
  | k' == k   = v
  | otherwise = findPtr k' kvs

------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])


