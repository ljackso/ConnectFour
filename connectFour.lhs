G52AFP Coursework 1 - Connect Four
   
Luke Jackson : psylj1@nottingham.ac.uk
Miles Plaskett : psymp2@nottingham.ac.uk

----------------------------------------------------------------------

> --import System.Random
> --import System.IO.Unsafe

For flexibility, we define constants for the row and column size of the
board, length of a winning sequence, and search depth for the game tree:

> rows			:: Int
> rows			=  6
>
> cols			:: Int
> cols			=  7
>
> win			:: Int
> win			=  4
>
> depth			:: Int
> depth			=  4

The board itself is represented as a list of rows, where each row is
a list of player values, subject to the above row and column sizes:

> type Board		=  [Row]
>
> type Row		=  [Player]

In turn, a player value is either a nought, a blank, or a cross, with
a blank representing a position on the board that is not yet occupied:

> data Player		=  O | B | X
> 			   deriving (Ord, Eq, Show)

The following code displays a board on the screen:

> showBoard		:: Board -> IO ()
> showBoard b		=  putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
> 			   where
>			      showRow = map showPlayer
>			      line    = replicate cols '-'
>			      nums    = take cols ['0'..]
>
> showPlayer		:: Player -> Char
> showPlayer O		=  'O'
> showPlayer B		=  '.'
> showPlayer X		=  'X'


This is the start of our code;

Various Test Boards

> test :: Board
> test = [[B, B, B, B, B, B, B ],
>         [B, B, B, B, B, B, B ],
>         [B, B, B, B, B, B, B ],
>         [B, B, B, X, X, B, B ],
>         [B, B, O, O, X, B, B ],
>         [O, O, O, X, X, X ,O ]]
>
> testWinningRow :: Board
> testWinningRow = [[B, B, B, B, B, B, B ],
>                  [B, B, B, B, B, B, B ],
>                  [B, B, B, B, B, B, B ],
>                  [B, B, B, X, X, B, B ],
>                  [O, O, O, O, X, B, B ],
>                  [X, O, O, X, X, X ,O ]]
>
> testWinningCol :: Board
> testWinningCol = [[B, B, B, B, B, B, B ],
>                  [B, B, B, B, B, B, B ],
>                  [B, O, B, B, B, B, B ],
>                  [B, O, B, X, X, B, B ],
>                  [B, O, O, O, X, B, B ],
>                  [X, O, O, X, X, X ,O ]]
>
> testWinningDia :: Board
> testWinningDia = [[B, B, B, B, B, B, B ],
>                  [B, B, B, B, B, B, B ],
>                  [B, B, B, X, B, B, B ],
>                  [B, B, X, O, B, B, B ],
>                  [B, X, X, O, B, B, B ],
>                  [X, O, O, O, X, B, B ]]
>
> testWinningDi2 :: Board
> testWinningDi2 = [[B, B, B, B, B, B, B ],
>                  [B, B, B, B, B, B, B ],
>                  [B, B, B, O, B, B, B ],
>                  [B, B, B, X, O, B, B ],
>                  [B, X, X, O, X, O, B ],
>                  [X, O, O, O, X, X, O ]]
>
> testWinningDi3 :: Board
> testWinningDi3 = [[B, B, B, B, B, B, B ],
>                  [B, B, B, B, B, B, B ],
>                  [B, B, B, B, X, B, B ],
>                  [B, B, B, X, O, B, B ],
>                  [B, B, X, O, X, B, B ],
>                  [X, X, O, O, X, O, O ]]

Creates an Empty Board to start he game 

> board :: Board 
> board = createEmptyBoard rows cols


Create a blank board:

> createEmptyBoard :: Int -> Int -> Board 
> createEmptyBoard 0 c = [] 
> createEmptyBoard r c = [createEmptyRow c] ++ createEmptyBoard (r-1) c 


Create a blank row:

> createEmptyRow :: Int -> Row
> createEmptyRow 0 = [] 
> createEmptyRow r = [B] ++ createEmptyRow (r-1)

Returns a row, 0 is the top then increments down:

> getRow :: Board -> Int -> Row
> getRow b r = b !! r


Returns a column in the same format as a row, 0 is the left column the increments across: 

> getColumn :: Board -> Int -> Row
> getColumn bs r = [ b !! r | b <- bs ]


Gives the row that you would add a new player if you were to drop on column c: 

> nextAvailableSpace :: Board -> Int -> Int
> nextAvailableSpace b c = (highestPlayer (getColumn b c)) - 1


Gets the position of the highest player in the column, the first occurrence of a player in a column:

> highestPlayer :: Row -> Int
> highestPlayer [] = rows
> highestPlayer (c:cs) = if positionIsEmpty c then highestPlayer cs else (rows - (length (c:cs)))


Says whether a position is empty:

> positionIsEmpty :: Player -> Bool
> positionIsEmpty B = True
> positionIsEmpty _ = False


Add a new player to the board takes the given board and returns a new board with the added player, p
at the given column, c:

> addPlayerToBoard :: Board -> Int -> Player-> Board
> addPlayerToBoard b c p = take (nextAvailableSpace b c) b ++ [r] ++ drop ((nextAvailableSpace b c) + 1) b
>                           where r = addPlayerToRow (getRow b (nextAvailableSpace b c)) c p

Add a new player to a row at postion c:

> addPlayerToRow :: Row  -> Int -> Player -> Row
> addPlayerToRow r c p = take c r ++ [p] ++ drop (c + 1) r


CHECKING FOR WIN
Test this using the three test boards, for row, col and diagonal wins

Check for win in a single row, if returns B then is not winning if return X or O is that player who wins

> winningRow :: Row -> Player
> winningRow (r:rs) =  if (sameList xs) && (length xs == win) then r 
>                      else
>                           if (length xs < win) then B
>                           else winningRow rs  
>                      where
>                           xs = take win (r:rs)

Check all elements in a list are the same

> sameList :: Row -> Bool
> sameList [_] = True 
> sameList (r:rs) = if (r == head rs) && (r /= B) then sameList rs else False 


Check entire board for winning row

> winningRowInBoard :: Board -> Player
> winningRowInBoard [] = B
> winningRowInBoard b = if elem O xs then O
>                           else 
>                               if elem X xs then X
>                               else B
>                           where 
>                               xs = [ winningRow (getRow b n) | n <- [0..(rows-1)]]


Check entire board for winning col

> winningColInBoard :: Board -> Player
> winningColInBoard b = if elem O xs then O
>                       else 
>                           if elem X xs then X
>                           else B
>                       where 
>                           xs = [ winningRow (getColumn b n) | n <- [0..(cols-1)]]


Check entire board for diagonals

> winningDiagInBoard :: Board -> Player
> winningDiagInBoard b = if r == B then l else r
>                           where
>                               l = winningColInBoard (getDiagBoardLeft b)
>                               r = winningColInBoard (getDiagBoardRight b)


Gets a board that helps uus get diagonals

> getDiagBoardRight :: Board -> Board
> getDiagBoardRight b = [ (getListOfB rn) ++ (getRow b rn)| rn <- [0..botRow]]
>                   where
>                       botRow = (rows - 1)
>
> getDiagBoardLeft :: Board -> Board
> getDiagBoardLeft b = [ (getListOfB (botRow - rn)) ++ (getRow b rn)| rn <- [0..botRow]]
>                   where
>                       botRow = (rows - 1)


Get list of Bs of size n

> getListOfB :: Int -> Row
> getListOfB n = [ B | b <- [1..n]]


Check board for any winning things

> winningBoard :: Board -> Player
> winningBoard b = if r /= B then r 
>                  else 
>                       if c /= B then c
>                       else
>                           if d /= B then d
>                           else B
>                   where 
>                       r = winningRowInBoard b
>                       c = winningColInBoard b
>                       d = winningDiagInBoard b

ACTUALLY PLAYING THE GAME 

Shows the board then listens to user column num to drop in new player 

> main :: IO()
> main = do
>           putStrLn "Welcome to the connect 4 Game!"
>           putStrLn "-------------------------------"
>           putStrLn "The computer is O and you are X"
>           putStrLn "The computer goes first "
>           runGame board 
>
> runGame :: Board -> IO()
> runGame b =   if winningBoard b /= B then
>                   do
>                       showBoard b
>                       putStrLn "Player is the winner !"
>               else 
>                   if currentPlayer b == X then 
>                       do
>                           showBoard b
>                           putStrLn "Your Turn"
>                           putStrLn "Enter a valid Column : "
>                           col <- getLine
>                           runGame (playerTurn b col)
>                   else
>                       do
>                           showBoard b
>                           putStrLn "Computers Turn" 
>                           runGame (computerTurn b)            
>               
>
> playerTurn :: Board -> String -> Board
> playerTurn b col =    if (validCol col b) then (addPlayerToBoard b (asInt col) X)
>                       else b  


checks input is valid 

> validCol :: String -> Board -> Bool
> validCol i b =    if isInt i then 
>                       if (asInt i) >= 0 && (asInt i) < cols then
>                           if isColumnNotFull b (asInt i) then True
>                           else False
>                       else False
>                   else False
>


check if a value is an Int

> isInt :: String -> Bool
> isInt s = case reads s :: [(Integer, String)] of
>               [(_, "")] -> True
>               _         -> False 


getString as an Int

> asInt :: String -> Int
> asInt s = (read s :: Int)


THE AI

Gives back a board with a new O player added by the comp:

> computerTurn :: Board -> Board 
> computerTurn b = addPlayerToBoard b c O
>                       where
>                           c = choseColAI b


Gives back a the best suited col to drop a O

> choseColAI :: Board -> Int 
> --choseColAI b = (moves !! unsafePerformIO (randomRIO (0, l-1)))
> choseColAI b = (moves !! (l `div` 2))
>                   where moves = bestMoves b
>                         l = length moves

ODL AI

Run the minimax algorithm

> bestMoves :: Board -> [Int]
> bestMoves b = [ (getNonEmptyCols b) !! i | i <- indexes ]
>               where minimaxArray = [ minimax board (depth-1) | board <- getAllPossibleBoards b O ]
>                     maxi = maximum minimaxArray
>                     indexes = indexOf maxi minimaxArray

> minimax :: Board -> Int -> Int
> minimax b d = if (d == 0) || (wb /= B) || (isBoardFull b) then
>                   if wb == O then 1
>                   else if wb == X then -1
>                   else 0
>               else
>                   if p == O then
>                       maximum minimaxArray
>                   else
>                       minimum minimaxArray
>               where
>                   p = currentPlayer b
>                   wb = winningBoard b
>                   minimaxArray = [ minimax board (d-1) | board <- getAllPossibleBoards b p ]

NEW AI

A data type for working with trees

> data Tree a = Node a [Tree a]


Gets an infintely large tree with

> getLargeTree :: Board -> Tree a
> getLargeTree b = Node b [getLargeTree nb | nb <- getAllPossibleBoards b (currentPlayer b)]


Prunes the big tree

> pruneTree :: Tree a -> Int -> Tree a
> pruneTree t d = 
> 


Gets a list of all possible boards given the current board and player turn

> getAllPossibleBoards :: Board -> Player -> [Board]
> getAllPossibleBoards b p = [ addPlayerToBoard b c p | c <- getNonEmptyCols b]


Gets a list of all columns that aren't full

> getNonEmptyCols :: Board -> [Int]
> getNonEmptyCols b = [ c | c <- [0..(cols - 1)], isColumnNotFull b c]


Check if a column is full

> isColumnNotFull :: Board -> Int -> Bool
> isColumnNotFull b c = if elem B col then True else False
>                       where col = getColumn b c


Check if board is full

> isBoardFull :: Board -> Bool
> isBoardFull b = countEmptySpaces b == 0


Get the current player turn

> currentPlayer :: Board -> Player
> currentPlayer b = if even (rows*cols - (countEmptySpaces b)) then O else X


Count the total empty spaces on the board

> countEmptySpaces :: Board -> Int
> countEmptySpaces b = sum [1 | r <- b, p <- r, p == B]


Get all indexes of an element in an array

> indexOf :: Int -> [Int] -> [Int]
> indexOf i arr = [ x | x <- [0..(length arr -1)], arr !! x == i ]

----------------------------------------------------------------------






