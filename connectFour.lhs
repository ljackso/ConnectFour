G52AFP Coursework 1 - Connect Four
   
Luke Jackson : psylj1@nottingham.ac.uk
Miles Plaskett : / Add email Millez08@wow.suchco.manyuk/

----------------------------------------------------------------------

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
> depth			=  6

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

Test Start Board

> test :: Board
> test = [[B, B, B, B, B, B, B ],
>         [B, B, B, B, B, B, B ],
>         [B, B, B, B, B, B, B ],
>         [B, B, B, X, X, B, B ],
>         [B, B, O, O, X, B, B ],
>         [B, O, O, X, X, X ,O ]]


Creates an Empty Board to start he game 

> board :: Board 
> board = createEmptyBoard rows cols

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


Create a blank board:

> createEmptyBoard :: Int -> Int -> Board 
> createEmptyBoard 0 c = [] 
> createEmptyBoard r c = [createEmptyRow c] ++ createEmptyBoard (r-1) c 


Create a blank row:

> createEmptyRow :: Int -> Row
> createEmptyRow 0 = [] 
> createEmptyRow r = [B] ++ createEmptyRow (r-1)
 


----------------------------------------------------------------------





