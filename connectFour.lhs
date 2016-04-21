> import System.IO
> import System.IO.Unsafe
> import System.Random
> import Data.List
> import Data.Ord


------------------------------------------------------------------------------------------------------------------

 DATA STRUCTURES USED

------------------------------------------------------------------------------------------------------------------


For flexibility, we define constants for the row and column size of the
board, length of a winning sequence, and search depth for the game tree:

> rows							:: Int
> rows							=  6
>
> cols							:: Int
> cols							=  7
>
> win							:: Int
> win							=  4

The board itself is represented as a list of rows, where each row is
a list of player values, subject to the above row and column sizes:
		
> type Board					=  [Row]
>			
> type Row		    			=  [Player]


In turn, a player value is either a nought, a blank, or a cross, with
a blank representing a position on the board that is not yet occupied:

> data Player					=  O | B | X
> 			   						deriving (Ord, Eq, Show)

> aiPlayer						:: Player
> aiPlayer						= O

> realPlayer					:: Player 
> realPlayer 					= X

The following code displays a board on the screen:
	
> showBoard						:: Board -> IO ()
> showBoard b					=  putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
> 			   						where
>			      						showRow = map showPlayer
>			      						line    = replicate cols '-'
>			      						nums    = take cols ['0'..]
>
> showPlayer					:: Player -> Char
> showPlayer O					=  'O'
> showPlayer B					=  '.'
> showPlayer X					=  'X'

------------------------------------------------------------------------------------------------------------------

 BUILDING AND ADDING TO A BOARD

------------------------------------------------------------------------------------------------------------------

Sets an Empty Board to start the game 

> board 						:: Board 
> board 						= createEmptyBoard rows cols

Create a blank board:

> createEmptyBoard 				:: Int -> Int -> Board 
> createEmptyBoard 0 c 			= [] 
> createEmptyBoard r c 			= [createEmptyRow c] ++ createEmptyBoard (r-1) c 


Create a blank row:

> createEmptyRow 				:: Int -> Row
> createEmptyRow 0 				= [] 
> createEmptyRow r 				= [B] ++ createEmptyRow (r-1)


Returns a row, 0 is the top then increments down:

> getRow 						:: Board -> Int -> Row
> getRow b r 					= b !! r


Returns a column in the same format as a row, 0 is the left column the increments across: 

> getColumn 					:: Board -> Int -> Row
> getColumn bs r 				= [ b !! r | b <- bs ]


Gives the row that you would add a new player if you were to drop on column c: 

> nextAvailableSpace 			:: Board -> Int -> Int
> nextAvailableSpace b c 		= (highestPlayer (getColumn b c)) - 1


Gets the position of the highest player in the column, the first occurrence of a player in a column:

> highestPlayer 				:: Row -> Int
> highestPlayer cs 				= maximum [p + 1 | p <- [0..(rows - 1)], cs !! p  == B]

Says whether a position is empty:

> positionIsEmpty 				:: Player -> Bool
> positionIsEmpty B 			=   True
> positionIsEmpty _ 			=   False

Gets a list of all the avialable indexes.

> getPossibleMoves 				:: Board -> [Int]
> getPossibleMoves b 			= [c | c <- [0.. (cols -1)], isColumnNotFull b c]


Add a new player to the board takes the given board and returns a new board with the added player, p
at the given column, c:


> addPlayerToBoard 				:: Board -> Int -> Player-> Board
> addPlayerToBoard b c p 		=  take na b ++ [addPlayerToRow (getRow b na) c p] ++ drop (na + 1) b
>									where
>										na = (nextAvailableSpace b c)

Add a new player to a row at postion c:

> addPlayerToRow 				:: Row  -> Int -> Player -> Row
> addPlayerToRow r c p  		= take c r ++ [p] ++ drop (c + 1) r

------------------------------------------------------------------------------------------------------------------

	CHECKING FOR A WIN

------------------------------------------------------------------------------------------------------------------

Test this using the three test boards, for row, col and diagonal wins. Our checker is a general checker that checks not only
if a player has won, but also which player. It basically returns  B to siginfy no win and X or O to siginify which player has
won.

Check for win in a single row, if returns B then is not winning if return X or O is that player who wins

> winningRow 					:: Row -> Player
> winningRow rs  
>           					| isInfixOf (minWin win X) rs 			= X
>           					| isInfixOf (minWin win O) rs           = O
>           					| otherwise                             = B  
>

Builds a minimum winning tree

> minWin 						:: Int -> Player -> [Player]
> minWin r p 					= [p | x <- [0..(r - 1)]]


Check all elements in a list are the same

> sameList 						:: Row -> Bool
> sameList [_] 					= True 
> sameList (r:rs) 				= if (r == head rs) && (r /= B) then sameList rs else False 


Check entire board for winning row

> winningRowInBoard 			:: Board -> Player
> winningRowInBoard [] 			= B
> winningRowInBoard b  
>               				|elem O xs = O
>               				|elem X xs = X
>               				|otherwise = B
>                  					where 
>                       				xs = [ winningRow (getRow b n) | n <- [0..(rows-1)]]


Check entire board for winning col, giving a start column and end column to start checkng through eg for a normal board 
would be 0 and cols - 1.

> winningColInBoard 			:: Board -> Int -> Int -> Player
> winningColInBoard b sc ec 
>               				| elem O xs = O
>               				| elem X xs = X
>               				| otherwise = B
>                  					where 
>                       				xs = [ winningRow (getColumn b n) | n <- [sc..ec]]
>                       


Check entire board for diagonals, deone by getting two boards, one the checks for left diagonals and one
that is checking for right by indexing a certian amount at each row and then checking for columns.
start is win - 1 because it gives the position you want to start checking from whhics 

> winningDiagInBoard 			:: Board -> Player
> winningDiagInBoard b 			= if r == B then l else r
>                           		where
>                              			l = winningColInBoard (getDiagBoardLeft b) start end
>                              			r = winningColInBoard (getDiagBoardRight b) start end
>                              			start = win - 1
>                              			end = (cols + rows - win - 1)


Gets a board that helps us get diagonals

> getDiagBoardRight 			:: Board -> Board
> getDiagBoardRight b 			= [ (getListOfB rn) ++ (getRow b rn) ++ (getListOfB (botRow - rn)) | rn <- [0..botRow]]
>                           	where
>                              		botRow = (rows - 1)
>
> getDiagBoardLeft 				:: Board -> Board
> getDiagBoardLeft b 			=  [ (getListOfB (botRow - rn)) ++ (getRow b rn)  ++ (getListOfB (rn))| rn <- [0..botRow]]
>                           		where
>                              			botRow = (rows - 1)

Get list of Bs of size n to help us construct the diagonal checker boards

> getListOfB 					:: Int -> Row
> getListOfB n 					= [ B | b <- [1..n]]


Check board for any winning things

> winningBoard 					:: Board -> Player
> winningBoard b 
>           					| r /= B    = r 
>           						| c /= B    = c
>           					| d /= B    = d
>           					| otherwise = B
>              						where 
>                   					r = winningRowInBoard b
>                   					c = winningColInBoard b 0 (cols -1)
>                   					d = winningDiagInBoard b

------------------------------------------------------------------------------------------------------------------

	PLAYING THE GAME 

------------------------------------------------------------------------------------------------------------------

Shows the board then listens to user column num to drop in new player, O is computer and goes first. 

> main 							:: IO()
> main 							= do
>           						putStrLn ""
>           						putStrLn "Welcome to the connect 4 Game!"
>           						putStrLn "-------------------------------"
>           						putStrLn "The computer is O and you are X"
>           						putStrLn "The computer always goes first "
>           						putStrLn "Call runGameMinMax to play against a min max AI"
>           						putStrLn "Call runGameMonteCarlo to play against a Monte Carlo AI"
>           						putStrLn "Call aiDeathMatch to watch the AI's battle it out (O will be Min Max and X Monte Carlo)"
>           						putStrLn ""


The Generic end game message

> endGameMessage				:: Board -> IO()
> endGameMessage b 				= 	do
>                       				showBoard b
>                       				if winningBoard b == X then 
>											do
>                              					putStrLn "X is the winner ! Press enter to exit"
>                              					abc <- getLine
>                              					putStrLn "Goodbye"
>                       				else 
>                          					do
>                              					putStrLn "0 is the winner ! Press enter to exit"
>                              					easyAs123 <- getLine
>                              					putStrLn "Goodbye"

A Generic message if it's a draw

> drawMessage					:: Board -> IO()
> drawMessage b 				=	do 
>                       	   			showBoard b
>                       	   			putStrLn "It's a draw, how boring! Press enter to exit"
>                          				simpleAsDoeRayMe <- getLine
>                          				putStrLn "Goodbye"

Run the game against minimax AI 

> runGameMinMax 				:: IO()          						 
> runGameMinMax 				= runGameMM board

> runGameMM 					:: Board -> IO()
> runGameMM b 					=   if winningBoard b /= B then
>										endGameMessage b
>               					else
>                   					if isBoardFull b then
>											drawMessage b
>                  						else   
>                      						if currentPlayer b == X then 
>                          						do
>              	           		     				showBoard b
>                          	    	 				putStrLn "Your Turn"
>                               					putStrLn "Enter a valid Column : "
>                               					col <- getLine
>                               					runGameMM (playerTurn b col)
>                       					else
>                           					do
>                               					showBoard b
>                               					putStrLn "Min Max Turn" 
>                               					runGameMM (computerTurnMM b)            


Run the game against the monte carlo AI

> runGameMonteCarlo 			:: IO()
> runGameMonteCarlo				= runGameMC board

> runGameMC 					:: Board -> IO()
> runGameMC b 					=   if winningBoard b /= B then
>										endGameMessage b
>               					else
>                   					if isBoardFull b then
>											drawMessage b
>                  						else   
>                      						if currentPlayer b == X then 
>                          						do
>              	           		     				showBoard b
>                          	    	 				putStrLn "Your Turn"
>                               					putStrLn "Enter a valid Column : "
>                               					col <- getLine
>                               					runGameMC (playerTurn b col)
>                       					else
>                           					do
>                               					showBoard b
>                               					putStrLn "Monte Carlos Turn" 
>                               					runGameMC (computerTurnMC b) 

Run the game with both AI's playin

> aiDeathMatch 					:: IO()
> aiDeathMatch					= runGameDM board

> runGameDM 					:: Board -> IO()
> runGameDM b 					=   if winningBoard b /= B then
>										endGameMessage b
>               					else
>                   					if isBoardFull b then
>											drawMessage b
>                  						else   
>                      						if currentPlayer b == X then 
>                          						do
>                               					showBoard b
>                               					putStrLn "Monte Carlos Turn" 
>                               					runGameDM (computerTurnMC b)
>                       					else
>                           					do
>                               					showBoard b
>                               					putStrLn "Min Max Turn" 
>                               					runGameDM (computerTurnMM b)                            

Handles the players turn

> playerTurn 					:: Board -> String -> Board
> playerTurn b col 				= 	if (validCol col b) then (addPlayerToBoard b (asInt col) X)
>                       			else b


checks input is valid 

> validCol 						:: String -> Board -> Bool
> validCol i b  
>           					| not (isInt i)                                 = False
>           					| not ((asInt i) >= 0 && (asInt i) < cols)      = False
>           					| isColumnNotFull b (asInt i)                   = True 
>           					| otherwise                                     = False
 
 
check if a value is an Int

> isInt 						:: String -> Bool
> isInt s 						= case reads s :: [(Integer, String)] of
>               					[(_, "")] -> True
>               					_         -> False 


getString as an Int

> asInt 						:: String -> Int
> asInt s 						= (read s :: Int)

------------------------------------------------------------------------------------------------------------------

	THE AI

------------------------------------------------------------------------------------------------------------------

Gives back a board with a new O player added by the computer,

> computerTurnMM 				:: Board -> Board 
> computerTurnMM b 				= addPlayerToBoard b c (currentPlayer b)
>                       				where
>                           				c = choseColMinMaxAI b
	
> computerTurnMC 				:: Board -> Board 
> computerTurnMC b 				= addPlayerToBoard b c (currentPlayer b)
>                       			where
>                           			c = choseColMCAI b


------------------------------------------------------------------------------------------------------------------

	MINIMAX AI

------------------------------------------------------------------------------------------------------------------


Gives back a the best suited col to drop a O, uses a random number if more than one option

> choseColMinMaxAI 				:: Board -> Int 
> choseColMinMaxAI b 			= 	if (l > 1) then moves !! randomNum l else head moves 
>                   					where 
>                       					moves = bestMoves b 
>                       					l = length moves


A data type for working with trees

> data Tree a 					= Node a [Tree a]
>                   				deriving (Ord, Eq, Show)

Depth of the min max tree 

> depth							:: Int
> depth							=  5


This returns a list of the best moves avaliable to the computer, this is done by getting the moves with the highest minimax value.
There may still be more than one even though they are ranked by depth. IT finds the minimax value by pruning an infinitly large tree to the
right depth. Then it peforms the minimax alogorithm on the first depth of each node. 

> bestMoves 					:: Board -> [Int]
> bestMoves b 					=  [(getNonEmptyCols b) !! i | i <- indexes,  isColumnNotFull b ((getNonEmptyCols b) !! i)]
>                       				where 
>                           				minimaxArray = [ (minimax (prune (depth - 1) (getLargeTree b1))) (depth) | b1 <- getAllPossibleBoards b (currentPlayer b) ]
>                           				maxi = maximum minimaxArray
>                           				indexes = indexOf maxi minimaxArray
>

Minimax Algorithm
works by giving positive and negative values for winning positions, with positions at higher depths given more importance than at lower depths 

> minimax 						:: Tree Board -> Int -> Int
> minimax (Node b []) d 
>                   			| (wow == O)                = d 
>                   			| (wow == X)                = negate d
>                   			| otherwise                 = 0
>                       			where
>                           			wow = winningBoard b
>
> minimax (Node b t) d 
>                   			| (wow == O)                = d 
>                   			| (wow == X)                = negate d
>                   			| otherwise                 = getMinimaxValue b [minimax n (d-1) | n <-t]
>                       			where
>                           			wow = winningBoard b   
>
>
> getMinimaxValue 				:: Board -> [Int] -> Int
> getMinimaxValue b (xs) 		= if (currentPlayer b) == O then maximum xs else minimum xs


Gets an infintely large tree

> getLargeTree 					:: Board -> Tree Board
> getLargeTree b 				= Node b [getLargeTree nb | nb <- getAllPossibleBoards b (currentPlayer b)]


Prunes a tree

> prune 						:: Int -> Tree a -> Tree a
> prune 0 (Node b _)    		= Node b []
> prune n (Node b xs)   		= Node b (map (prune (n-1)) xs)


Gets a list of all possible boards given the current board and player turn

> getAllPossibleBoards 			:: Board -> Player -> [Board]
> getAllPossibleBoards b p 		= [ addPlayerToBoard b c p | c <- getNonEmptyCols b]


Gets a list of all columns that aren't full

> getNonEmptyCols 				:: Board -> [Int]
> getNonEmptyCols b 			= [ c | c <- [0..(cols - 1)], isColumnNotFull b c]


Check if a column is full

> isColumnNotFull 				:: Board -> Int -> Bool
> isColumnNotFull b c   		=  elem B (getColumn b c) 


Check if board is full

> isBoardFull 					:: Board -> Bool
> isBoardFull b 				= countEmptySpaces b == 0


Get the current player turn

> currentPlayer 				:: Board -> Player
> currentPlayer b 				= if even (rows*cols - (countEmptySpaces b)) then O else X


Count the total empty spaces on the board

> countEmptySpaces 				:: Board -> Int
> countEmptySpaces b 			= sum [1 | r <- b, p <- r, p == B]


Get all indexes of an element in an array

> indexOf 						:: Int -> [Int] -> [Int]
> indexOf i arr 				= [ x | x <- [0..(length arr -1)], arr !! x == i ]

----------------------------------------------------------------------

	MONTECARLO SIMULATIONS AI

----------------------------------------------------------------------

> maxNumberSimulations 			:: Int
> maxNumberSimulations 			= 1000

Gives back a the best suited col to drop a O, uses a random number if more than one option

> choseColMCAI 					:: Board -> Int 
> choseColMCAI b 				=  snd (maximumBy (comparing fst) results)
>									where
>										results = [(monteCarloStartPos b c simsPerAttempt, c) | c <- [0..(cols - 1)], isColumnNotFull b c]
>										simsPerAttempt = div maxNumberSimulations (length results)

Simulates a game choosing purely random moves takes in a board, and which colum they start at. Then returns the number of succesful simulations.

> monteCarloStartPos			:: Board -> Int -> Int -> Int 
> monteCarloStartPos b c n		=  length (filter (==(currentPlayer b)) (multipltSimGame n b (currentPlayer b) c))

Simulate a game untill you have a result. Start by including the intial start point

> multipltSimGame 				:: Int -> Board -> Player -> Int -> [Player] 
> multipltSimGame s b p c 		= [simulateGame (addPlayerToBoard b c p) | x <- [0..s]]
>
> simulateGame 					:: Board -> Player
> simulateGame b   				= 	if winner == B then 
>										if isBoardFull b then 
>											B
>										else
>											simulateGame (addPlayerToBoard b (randNumFromList(getPossibleMoves b)) (currentPlayer b))
>									else
>										winner 
>												where 
>													winner = winningBoard b


----------------------------------------------------------------------

	 UTILS FOR WORKING WITH RANDOM NUMBERS

----------------------------------------------------------------------

Random function used to help select a postion randomly if several positions have the same minimax value

> randomNum    					:: Int -> Int
> randomNum n   				=  unsafePerformIO (randomRIO (0,n-1))

Gets a random number from a list of numbers

> randNumFromList 				:: [Int] -> Int
> randNumFromList xs 			= xs !! randomNum (length xs) 




