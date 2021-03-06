import Data.Maybe
import Data.List

type AttName = String

type AttValue = String

type Attribute = (AttName, [AttValue])

type Header = [Attribute]

type Row = [AttValue]

type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue | 
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

xlogx :: Double -> Double
xlogx p
  | p <= 1e-100 = 0.0
  | otherwise   = p * log2 p 
  where
    log2 x = log x / log 2

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp x table
  = fromMaybe (error ("lookUp error - no binding for " ++ show x ++ 
                      " in table: " ++ show table))
              (lookup x table)

--------------------------------------------------------------------
-- PART I
--------------------------------------------------------------------

allSame :: Eq a => [a] -> Bool
allSame []
  = True
allSame (x:xs)
  = and [x' == x | x' <- xs]

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove _ []
  = []
remove x (kv@(k, v) : kvs)
  | x == k    = kvs
  | otherwise = kv : (remove x kvs)

lookUpAtt :: AttName -> Header -> Row -> AttValue
--Pre: The attribute name is present in the given header.
lookUpAtt attName header row
  = row !! lookUp attName (zip (map fst header) [0..])

removeAtt :: AttName -> Header -> Row -> Row
removeAtt attName header row
  = map snd (remove attName (zip (map fst header) row))

addToMapping :: Eq a => (a, b) -> [(a, [b])] -> [(a, [b])]
addToMapping (x, v) []
  = [(x, [v])]
addToMapping (x, v) ((k, vs) : kvs)
  | x == k = (k, v : vs) : kvs
  | otherwise = (k, vs) : (addToMapping (x, v) kvs)

buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
--Pre: Each row of the data set contains an instance of the attribute
buildFrequencyTable (attName, attVals) (header, rows)
  = [(attVal, countOccurences attVal) | attVal <- attVals]
  where
    countOccurences :: AttValue -> Int
    countOccurences val
      = length (filter (==True) [(lookUpAtt attName header row) == val | row <- rows])
-- Inefficient: traverse the row for each possible attribute value

--------------------------------------------------------------------
-- PART II
--------------------------------------------------------------------

nodes :: DecisionTree -> Int
nodes (Null)
  = 0
nodes (Leaf _)
  = 1
nodes (Node _ branches)
  = 1 + sum (map nodes (map snd branches))

evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree (Null) _ _
  = ""
evalTree (Leaf result) _ _
  = result
evalTree (Node attName branches) header row
  = evalTree (lookUp attVal branches) header row
    where
      attVal = lookUpAtt attName header row 

--------------------------------------------------------------------
-- PART III
--------------------------------------------------------------------

--
-- Given...
-- In this simple case, the attribute selected is the first input attribute 
-- in the header. Note that the classifier attribute may appear in any column,
-- so we must exclude it as a candidate.
--
nextAtt :: AttSelector
--Pre: The header contains at least one input attribute
nextAtt (header, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) header)

partitionData :: DataSet -> Attribute -> Partition
partitionData (header, rows) (attName, attVals)
  = [(attVal, partitionData' attVal) | attVal <- attVals]
  where
    -- helper function, form the dataset for an attribute value
    partitionData' :: AttValue -> DataSet
    partitionData' attVal
      = (header', rows')
      where
        -- header' and rows' form the dataset for an attribute value
        header' = remove attName header
        rows' = map (removeAtt attName header) frows
        -- frows is the list of relevant rows (filtered on attVal equalling)
        frows = filter ( ((==) (attVal)) . (lookUpAtt attName header)) rows

buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree 
buildTree dS@(header, rows) cAtt@(cName, _) fSel
  | rows == [] = Null
  | allSame (map (lookUpAtt cName header) rows)
      = Leaf (lookUpAtt cName header (rows !! 0))
  | otherwise  = Node attName childTrees
  where
    -- this node's attribute
    att@(attName, _) = fSel dS cAtt
    -- partition for this attribute
    part = partitionData dS att
    -- recursively build child trees
    -- partition is [(AttValue, DataSet)], we want [(AttValue, DecisionTree)]
    childTrees = [(attVal, buildTree dS' cAtt fSel) | (attVal, dS') <- part]

--------------------------------------------------------------------
-- PART IV
--------------------------------------------------------------------

entropy :: DataSet -> Attribute -> Double
entropy (_, []) _
  = 0.0
entropy dataSet@(header, rows) att@(attName, attVals)
  = sum [0 - xlogx (prob p) | p <- attVals]
  where
    freqTable = buildFrequencyTable att dataSet
    prob x = (fromIntegral (lookUp x freqTable)) / (fromIntegral (length rows))

gain :: DataSet -> Attribute -> Attribute -> Double
gain dataSet@(header, rows) att@(attName, attVals) cAtt
  = edc - sum [(prob attVal) * (entropy (dS' attVal) cAtt) | attVal <- attVals]
  where
    edc = entropy dataSet cAtt
    freqTable = buildFrequencyTable att dataSet
    prob x = (fromIntegral (lookUp x freqTable)) / (fromIntegral (length rows))
    dS' attVal = lookUp attVal (partitionData dataSet att)

bestGainAtt :: AttSelector
-- map the gain function to the list of attValues given in header
-- zip that with header, to get a list of [(Double, Attribute)]
-- return the attribute with the biggest gain
bestGainAtt dataSet@(h, _) cAtt@(cName, _)
  = (snd . getMax) (zip (map (flip (gain dataSet) cAtt) (remove cName h)) h)

getMax :: (Ord a) => [(a, b)] -> (a, b)
-- pre: non-empty list
getMax ((k, v) : [])
  = (k, v)
getMax ((k, v) : kvs)
  | k > k'    = (k, v)
  | otherwise = (k', v')
  where
    (k', v') = getMax kvs

--------------------------------------------------------------------

outlook :: Attribute
outlook 
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute 
temp 
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute 
humidity 
  = ("humidity", ["high", "normal"])

wind :: Attribute 
wind 
  = ("wind", ["windy", "calm"])

result :: Attribute 
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header 
  =  [outlook,    temp,   humidity, wind,    result] 
table 
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- This is the same as the above table, but with the result in the second 
-- column...
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header' 
  =  [outlook,    result, temp,   humidity, wind] 
table' 
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

fig1 :: DecisionTree
fig1
  = Node "outlook" 
         [("sunny", Node "temp" 
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity" 
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp" 
                         [("hot", Null),
                          ("mild", Node "humidity" 
                                        [("high",Node "wind" 
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity" 
                                        [("high", Null),
                                         ("normal", Node "wind" 
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook" 
         [("sunny", Node "humidity" 
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind" 
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]


outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]
