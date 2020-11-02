{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import Data.Maybe
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node
	{ state :: s
	, action :: Maybe a
	, depth :: Int
	, parent :: Maybe (Node s a)
	, children :: [Node s a]
	} deriving Show
{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState = state

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent = parent

nodeDepth :: Node s a -> Int
nodeDepth = depth

nodeAction :: Node s a -> Maybe a
nodeAction =  action

nodeChildren :: Node s a -> [Node s a]
nodeChildren = children

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

makeChild :: (ProblemState s a, Eq s) => s -> a -> (Node s a) -> Int -> (Node s a)
makeChild s a parentCurr depthCurr = newChild
	where
		newChild = (Node s (Just a) depthCurr (Just parentCurr) childNodes)
		childNodes = map (\(newA, newS) -> (makeChild newS newA newChild (depthCurr + 1))) $ successors s

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace s = rootNode
	where
		rootNode = (Node s Nothing 0 Nothing childNodes)
		childNodes = map (\(newA, newS) -> (makeChild newS newA rootNode 1)) $ successors s

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs rootNode = result
	where
		frontier = nodeChildren rootNode
		result = ([rootNode], [rootNode]) : (frontier, frontier) : doBFS (head frontier) [rootNode] (tail frontier) 

-- Helper function
doBFS :: Ord s => (Node s a) -> [Node s a] -> [Node s a] -> [([Node s a], [Node s a])]
doBFS currNode visited frontier = (addedNodes, newFrontier) : doBFS (head newFrontier) (currNode : visited) (tail newFrontier)
	where
		member node temp = elem (nodeState node) $ map (nodeState) temp
		addedNodes = filter (not . (flip member) (currNode : visited)) (nodeChildren currNode)
		newFrontier = frontier ++ addedNodes	

{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS start end = intersection
	where
		parse startBFS endBFS = [(first, second) | first <- snd startBFS, second <- fst endBFS, nodeState first == nodeState second]
		bfsZip = zipWith (\x1 x2 -> parse x1 x2) (bfs start) (bfs end)
		intersection = head $ head $ dropWhile (null) bfsZip

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: Node s a -> [(Maybe a, s)]
extractPath (Node s Nothing _ _ _) = [(Nothing, s)]
extractPath (Node myS myA _ myP _) = (extractPath $ fromJust myP) ++ [(myA, myS)]


{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve start end = firstHalf ++ finalSecond
	where
		(first, second) = bidirBFS (createStateSpace start) (createStateSpace end)
		firstHalf = extractPath first
		secondHalf = reverse $ tail $ extractPath second
		reverseSecond = map (\(myA, myS) -> reverseAction $ (fromJust myA, myS)) secondHalf
		trueSecond = map (\(myA, myS) -> (Just myA, myS)) reverseSecond ++ [(Nothing, end)]
		finalSecond = zipWith (\x1 x2 -> (fst x1, snd x2)) trueSecond (tail trueSecond)