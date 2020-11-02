{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell 
	{ piece :: Char
	, xPos :: Int
	, yPos :: Int
	}
	deriving (Eq, Ord, Show)

{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level
	{ gameMap :: (A.Array (Int, Int) Cell)
	, bound :: Position
	}
    deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

instance Show Level 
    where 
		show (Level xs _ ) = reverse $ (foldl (\acc (Cell x _ _) -> x : acc) "\n" $ A.elems xs)

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel pos@(x,y) = Level {gameMap = newArray, bound = pos}
	where
		newArray = A.array ((0, 0), (x, y + 1)) expr
		expr = [if j < y + 1 then ((i, j), (Cell emptySpace i j)) else ((i, j), (Cell endl i j)) | i <- [0..x], j <- [0..y+1]]

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

addCell :: (Char, Position) -> Level -> Level
addCell (cellType, (x, y)) currLevel 
	| x >= 0 && y >= 0  && (x, y) <= (bound currLevel) = 
		let
			currMap = gameMap currLevel
			newMap = currMap A.// [((x, y), (Cell cellType x y))]
		in
			(Level newMap (bound currLevel))
	| otherwise = currLevel
		


{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel pos xs = foldr (addCell) newLevel xs
	where
		newLevel = emptyLevel pos


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}
 
moveCell :: Position -> Directions -> Level -> Level
moveCell (x, y) dir currLevel
	| elem currCell startCells = currLevel
	| elem currCell winningCells = currLevel 
	| currCell == endl = currLevel
	| dir == North && (isSafeX (x - 1)) && (isEmpty ((x - 1),y)) =
		addCell (emptySpace, (x, y)) $ addCell (currCell, (x - 1, y)) currLevel
	| dir == South && (isSafeX (x + 1)) && (isEmpty ((x + 1),y)) = 
		addCell (emptySpace, (x, y)) $ addCell (currCell, (x + 1, y)) currLevel
	| dir == West && (isSafeY (y - 1)) && (isEmpty (x,(y - 1))) = 
		addCell (emptySpace, (x, y)) $ addCell (currCell, (x, y - 1)) currLevel
	| dir == East && (isSafeY (y + 1)) && (isEmpty (x,(y + 1))) =
		addCell (emptySpace, (x, y)) $ addCell (currCell, (x, y + 1)) currLevel
	| otherwise = currLevel
	where
		isSafeX a = a >= 0 && a <= xBound
		isSafeY a = a >= 0 && a <= yBound
		xBound = fst (bound currLevel)
		yBound = snd (bound currLevel)
		currMap = gameMap currLevel
		isEmpty pos = (piece (currMap A.! pos)) == emptySpace
		currCell = piece (currMap A.! (x, y))

{-
    *** TODO ***

    Verifică dacă două celule se pot conecta.
    Atenție: Această funcție se aplică de la stânga la 
    dreapta(nu este comutativă).

    ex: connection botLeft horPipe = True (╚═)
        connection horPipe botLeft = False (═╚)
-}
connection :: Cell -> Cell -> Bool
connection (Cell pieceF xF yF) (Cell pieceS xS yS)
	| pieceF == verPipe && pieceS == verPipe = abs (xF - xS) == 1 && yF == yS
	| pieceF == horPipe && pieceS == horPipe = xF == xS && abs (yF - yS) == 1
	| (elem pieceF leftDir) && (elem pieceS rightDir) = xF == xS && yF + 1 == yS
	| (elem pieceF rightDir) && (elem pieceS leftDir) = xF == xS && yF - 1 == yS
	| (elem pieceF downDir) && (elem pieceS upDir) = xF + 1 == xS && yF == yS
	| (elem pieceF upDir) && (elem pieceS downDir) = xF - 1 == xS && yF == yS
	| otherwise = False
	where
		upDir = [verPipe, botLeft, botRight, startUp, winUp] -- pointing up
		downDir = [verPipe, topLeft, topRight, startDown, winDown] -- pointing down
		leftDir = [horPipe, botLeft, topLeft, startRight, winRight] -- pointing left
		rightDir = [horPipe, botRight, topRight, startLeft, winLeft] -- pointing right
	

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

-- Used to get the unique start position
getStart :: Level -> Cell
getStart currLevel = head $ [snd x | x <- (assocs (gameMap currLevel)),
			elem (piece (snd x)) startCells]

-- Used to get the unique win position
getWin :: Level -> Cell
getWin currLevel = head $ [snd x | x <- (assocs (gameMap currLevel)),
			elem (piece (snd x)) winningCells]

-- Used to reconstruct the path from the start to the end
checkPath :: Level -> Cell -> (Int, Int) -> Cell
checkPath currLevel myCell@(Cell _ currX currY) (lastX, lastY)
	| (currX - 1 >= 0) && (currX - 1 <= xBound) && (currX - 1 /= lastX) && 
		isConnected (currX - 1, currY) = checkPath currLevel (getNextCell (currX - 1, currY)) (currX, currY)
	| (currX + 1 >= 0) && (currX + 1 <= xBound) && (currX + 1 /= lastX) && 
		isConnected (currX + 1, currY) = checkPath currLevel (getNextCell (currX + 1, currY)) (currX, currY)
	| (currY - 1 >= 0) && (currY - 1 <= yBound) && (currY - 1 /= lastY) && 
		isConnected (currX, currY - 1) = checkPath currLevel (getNextCell (currX, currY - 1)) (currX, currY)
	| (currY + 1 >= 0) && (currY + 1 <= yBound) && (currY + 1 /= lastY) && 
		isConnected (currX, currY + 1) = checkPath currLevel (getNextCell (currX, currY + 1)) (currX, currY)
	| otherwise = myCell
	where
		xBound = fst (bound currLevel)
		yBound = snd (bound currLevel)
		currMap = gameMap currLevel
		getNextCell (someX, someY) = currMap A.! (someX, someY)
		isConnected (someX, someY) = connection myCell $ getNextCell (someX, someY)

wonLevel :: Level -> Bool
wonLevel currLevel = currPos == winPos
	where
		startPos = getStart currLevel
		winPos = getWin currLevel
		currPos = checkPath currLevel startPos (-1, -1)

-- Used to get the possible moves of a piece
possibleMoves :: Level -> Cell -> [((Position, Directions), Level)]
possibleMoves currLevel (Cell currPiece x y)
	| y == (yBound + 1) = []
	| elem currPiece startCells || elem currPiece winningCells || currPiece == emptySpace || currPiece == endl = []
	| otherwise = moveRight ++ moveLeft ++ moveDown ++ moveUp 
	where
		xBound = fst (bound currLevel)
		yBound = snd (bound currLevel)
		currMap = gameMap currLevel
		getNextCell (someX, someY) = currMap A.! (someX, someY)
		moveUp
			| (x - 1 >= 0) && (x - 1 <= xBound) && (piece (getNextCell (x - 1, y))) == emptySpace = [(((x, y), North), moveCell (x, y) North currLevel)]
			| otherwise = []
		moveDown
			| (x + 1 >= 0) && (x + 1 <= xBound) && (piece (getNextCell (x + 1, y))) == emptySpace = [(((x, y), South), moveCell (x, y) South currLevel)]
			| otherwise = []
		moveLeft
			| (y - 1 >= 0) && (y - 1 <= yBound) && (piece (getNextCell (x, y - 1))) == emptySpace = [(((x, y), West), moveCell (x, y) West currLevel)]
			| otherwise = []
		moveRight
			| (y + 1 >= 0) && (y + 1 <= yBound) && (piece (getNextCell (x, y + 1))) == emptySpace = [(((x, y), East), moveCell (x, y) East currLevel)]
			| otherwise = []	

instance ProblemState Level (Position, Directions) where
    successors currLevel@(Level xs _ ) = reverse $ foldl (++) [] $ map (possibleMoves currLevel) $ A.elems xs
    isGoal = wonLevel
    reverseAction (((x, y), dir), currLevel)
		| dir == North = (((x - 1, y), South), moveCell (x, y) South currLevel)
		| dir == South = (((x + 1, y), North), moveCell (x, y) North currLevel)
		| dir == West = (((x, y - 1), East), moveCell (x, y) East currLevel)
		| dir == East = (((x, y + 1), West), moveCell (x, y) West currLevel)
