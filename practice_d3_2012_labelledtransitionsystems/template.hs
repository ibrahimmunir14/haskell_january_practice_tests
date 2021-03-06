import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp key ((k, v) : kvs)
  = if key == k then v else lookUp key kvs

states :: LTS -> [State]
states 
  = nub . states'
states' []
  = []
states' (((s, s'), _) : ts)
  = s : s' : states' ts

transitions :: State -> LTS -> [Transition]
transitions s []
  = []
transitions s (t@((s1, _), _) : ts)
  = if s == s1
    then t : transitions s ts
    else transitions s ts

alphabet :: LTS -> Alphabet
alphabet
  = nub . alphabet'
alphabet' []
  = []
alphabet' (((_, _), id) : ts)
  = id : alphabet' ts

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions (STOP)
  = []
actions (Ref _)
  = []
actions (Prefix action proc)
  = action : actions proc
actions (Choice procs)
  = foldl (++) [] (map actions procs)

accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts actions procDefs
  = accepts' actions (snd (head procDefs))
  where
    accepts' :: [Id] -> Process -> Bool
    accepts' [] _
      = True
    accepts' acts (STOP)
      = False
    accepts' acts (Ref r)
      = accepts' acts (lookUp r procDefs)
    accepts' (act : acts) (Prefix act' proc)
      = if act == act'
        then accepts' acts proc
        else False
    accepts' actions (Choice procs)
      = or (map (accepts' actions) procs)


------------------------------------------------------
-- PART III

--composeTransitions :: Transition -> Transition 
--                   -> Alphabet -> Alphabet 
--                   -> StateMap 
--                   -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions ((s, t), a) ((s', t'), a') alpha1 alpha2 map
  | a == a'                          = [((m s s', m t t'), a)]
  | elem a  alpha2 && elem a' alpha1 = []
  | elem a' alpha1                   = [((m s s', m t s'), a)]
  | elem a  alpha2                   = [((m s s', m s t'), a')]
  | otherwise                        = [((m s s', m t s'), a),
                                        ((m s s', m s t'), a')]
  where
    m s s' = lookUp (s, s') map

pruneTransitions :: [Transition] -> LTS
pruneTransitions ts
  = nub $ visit 0 []
  where
    visit :: State -> [State] -> [Transition]
    visit s vs
      | elem s vs = []
      | otherwise = outTs ++ concat [visit t (f : vs) | ((f, t), a) <- outTs]
      where
        outTs = transitions s ts

------------------------------------------------------
-- PART IV

compose :: LTS -> LTS -> LTS
compose lts lts'
  = pruneTransitions $ concat composed
  where
    composed = [composeT t t' alpha1 alpha2 stateMap | (t, t') <- tranPairs]
    composeT t@(_, a) t'@(_, a') alpha alpha' sMap
      = composeTransitions t t' alphaN alphaN' sMap
      where
        alphaN  = if a' == "$'" then ("$'" : alpha) else alpha
        alphaN' = if a  == "$"  then ("$" : alpha') else alpha'
    alpha1    = alphabet lts
    alpha2    = alphabet lts'
    newStates = [(s, s') | s <- states lts, s' <- states lts']
    stateMap  = zip newStates [0..]
    tranPairs = [(t, t') | (s, s') <- newStates, t <- getTrans s lts s' "$", t' <- getTrans s' lts' s "$'"]
    getTrans from ltSystem to sentinel
      | transitions from ltSystem /= [] = transitions from ltSystem
      | otherwise                       = [((from, to), sentinel)]
------------------------------------------------------
-- PART V

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS 
  = undefined

------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor 
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock 
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play 
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), 
                     Prefix "end" STOP])

maker 
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch 
  = ("SWITCH", Ref "OFF")

off 
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS, 
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS 
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS 
  = [((0,1),"tick"),((1,0),"tock")]

playLTS 
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS 
  = [((0,1),"make"),((1,0),"ready")]

userLTS 
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS 
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS 
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS 
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS 
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS 
  = [((0,1),"on"),((1,0),"off")]

