import Data.List
import Data.Char
import Test.HUnit 

{- Pieces
   A datatype that show that a cell can be either 
   empty with a number or occupied with a player X or O.
  -}
data Pieces = Empty Int | Player Char deriving (Eq, Read)

{- Show Pieces
     Hides Empty and Player since we don't want to see it in the board.
     RETURNS: A string value whatever if the input is Int or Char
     EXAMPLES: show (Empty 2) = "2"
               show (Player 'X') = "X"
  -}
instance Show Pieces where 
  show (Empty i) = show i
  show (Player c) = [c]

-----------------------------------------------------------

{- main
     An IO function that shows the beginning of the game 
     with prompting player X to start.
     SIDE EFFECTS: Prints out some sentences and the board. 
  -}
main :: IO ()
main = do
  putStrLn "Welcome to the TicTacToe game made in Haskell!"
  putStrLn "Player X goes first. Choose a number from 1-9 to place your piece."
  putStrLn ""
  gameLoop newBoard 'X'

{- newBoard
     Creates a list with numbers from 1 to 9
     PRE:  
     RETURNS: A list containing numbers from 1 to 9 
     SIDE EFFECTS: 
     EXAMPLES: newBoard = [1,2,3,4,5,6,7,8,9]
  -}
newBoard :: [Pieces]
newBoard = map Empty [1..9]

{- gameLoop board player
   A gameloop on how the game should be played 
   from beginning to end, shows who wins, 
   if the game ends in a draw 
   and if an invalid move has been made.
     SIDE EFFECTS: A lot, inputs is taken and many things get printed out.
  -}
gameLoop :: [Pieces] -> Char -> IO ()
gameLoop board player = do
  emptyBoard board
  putStrLn $ "Player " ++ [player] ++ ", write your number down below:"
  move <- getLine
  let num = read move :: Int
  if isValid board num then do
    let newBoard = placePiece board (Player player) num
    putStrLn ""
    if isWinner newBoard then do
      emptyBoard newBoard
      putStrLn $ " Congratulations player " ++ [player] ++ ", you won!"
    else if isDraw newBoard then do
      emptyBoard newBoard
      putStrLn "It's a draw!, which means that no one won :("
    else do
      let next = nextPlayer player
      gameLoop newBoard next
  else do
    putStrLn ""
    putStrLn "Invalid move, try again and type a number between 1 to 9 please."
    gameLoop board player

--------------------------------------------------------------------------------

{- guardsInBetween row
     Puts guards inbetween the elements in the list.
     RETURNS: A string with elements of a list and with guards inbetween.
     EXAMPLES: guardsInBetween [Empty 1, Player 'X', Empty 3] = " 1 | X | 3 "
  -}
guardsInBetween :: [Pieces] -> String
guardsInBetween row = intercalate " | " $ fmap show row

{- dividingLine
     A string 
     RETURNS: A string
  -}
dividingLine :: String
dividingLine = "---------"

{- emptyBoard board
   Prints out an empty board that looks like the board down below.
   1 | 2 | 3
   ---------
   4 | 5 | 6
   ---------
   7 | 8 | 9  
     SIDE EFFECTS: Prints out an empty gameboard.
  -}
emptyBoard :: [Pieces] -> IO ()
emptyBoard board = do
  putStrLn $ guardsInBetween firstRow
  putStrLn dividingLine
  putStrLn $ guardsInBetween secondRow
  putStrLn dividingLine
  putStrLn $ guardsInBetween thirdRow
  where firstRow = take 3 board
        secondRow = take 3 $ drop 3 board
        thirdRow = drop 6 board

{- placePiece board piece num
     Puts a piece on the board.
     RETURNS: A new list that's updated with the piece.
     EXAMPLES: placePiece board (Player 'X') 5 = [1,2,3,4,X,6,7,8,9].
  -}
placePiece :: [Pieces] -> Pieces -> Int -> [Pieces]
placePiece board piece num = take (num - 1) board ++ [piece] ++ drop num board

{- isValid board num
     Checks whether an input is valid or not.
     RETURNS: True or False, depending on the input.
     EXAMPLES: isValid [Empty 1, Empty 2, Empty 3, Empty 4, 
                        Empty 5, Empty 6, Empty 7, Empty 8, Empty 9] 3 = True
               isValid [Empty 1, Empty 2, Empty 3, Empty 4, 
                        Player 'X', Empty 6, Empty 7, Empty 8, Empty 9] 5 = False
  -}
isValid :: [Pieces] -> Int -> Bool
isValid board num = num >= 1 && num <= 9 && case board !! (num - 1) of
                                              Empty _ -> True
                                              _       -> False

{- isDraw board
     Checks if the game is a tie/draw. 
     RETURNS: True or False 
     EXAMPLES: isDraw  [Player 'X', Player 'O', Player 'X', Player 'O', Player 'O', 
                        Player 'X', Player 'X', Player 'X', Player 'O'] = True
  -}
isDraw :: [Pieces] -> Bool
isDraw board = all isPlayer board && not (isWinner board)
  where isPlayer (Player _) = True
        isPlayer _         = False

{- isWinner board
    Checks if there is a winner.
     RETURNS: True if win or False is lose
     EXAMPLES: isWinner [Player 'X', Player 'X', Player 'X', Empty 4, 
                         Empty 5, Empty 6, Empty 7, Empty 8, Empty 9] = True
  -}
isWinner :: [Pieces] -> Bool
isWinner board = any isWinningRow rows || any isWinningRow columns || any isWinningRow diagonals
  where
    rows = [[0, 1, 2], [3, 4, 5], [6, 7, 8]]
    columns = [[0, 3, 6], [1, 4, 7], [2, 5, 8]]
    diagonals = [[0, 4, 8], [2, 4, 6]]
    isWinningRow row = all (\x -> board !! x == Player player) row
      where
        player = case map (\x -> board !! x) row of
          [Player t1, Player t2, Player t3] | t1 == t2 && t2 == t3 -> t1
          _ -> ' '

{- nextPlayer player
     Switch from a player to the next when playing  
     RETURNS: The Char representing the next player ('X' or 'O')
     EXAMPLES: nextPlayer 'X' = 'O'
               nextPlayer 'O' = 'X'
  -}
nextPlayer :: Char -> Char
nextPlayer 'X' = 'O'
nextPlayer 'O' = 'X'

-------------------------------------------------

-- Tests Cases (To run it , type runTestTT tests)
test0 = TestCase (assertEqual "for (guardsInBetween [Empty 1 , Empty 2 ,  Player 'X' ]), "
                               "1 | 2 | X" 
                               (guardsInBetween [Empty 1 , Empty 2 ,  Player 'X' ]))

test1 = TestCase (assertEqual "for (isValid [Empty 0, Empty 0, Empty 0, Empty 0, Empty 0, Empty 0, Empty 0, Empty 0, Empty 0] 3),"
                              True
                              (isValid [Empty 0, Empty 0, Empty 0, Empty 0, Empty 0, Empty 0, Empty 0, Empty 0, Empty 0] 3))

test2 = TestCase (assertEqual "for (isValid [Empty 0, Empty 0, Empty 0, Empty 0, Player 'X', Empty 0, Empty 0, Empty 0, Empty 0] 5),"
                              False
                              (isValid [Empty 0, Empty 0, Empty 0, Empty 0, Player 'X', Empty 0, Empty 0, Empty 0, Empty 0] 5))

test3 = TestCase (assertEqual "for (isDraw [Player 'X', Player 'O', Player 'X', Player 'O', Player 'O', Player 'X', Player 'X', Player 'X', Player 'O']),"
                              True
                              (isDraw [Player 'X', Player 'O', Player 'X', Player 'O', Player 'O', Player 'X', Player 'X', Player 'X', Player 'O']))

test4 = TestCase (assertEqual "for (isWinner [Player 'X', Player 'X', Player 'X', Empty 4, Empty 5, Empty 6, Empty 7, Empty 8, Empty 9]),"
                              True
                              (isWinner [Player 'X', Player 'X', Player 'X', Empty 4, Empty 5, Empty 6, Empty 7, Empty 8, Empty 9]))

test5 = TestCase (assertEqual "for (isWinner [Empty 1, Empty 2, Player 'O', Empty 4, Player 'O', Player 'O', Player 'X', Empty 8, Empty 9]),"
                              False
                              (isWinner [Empty 1, Empty 2, Player 'O', Empty 4, Player 'O', Player 'O', Player 'X', Empty 8, Empty 9]))

test6 = TestCase (assertEqual "for (isWinner [Empty 1, Empty 2, Player 'O', Empty 4, Player 'X', Player 'X', Player 'X', Player 'O', Empty 9]),"
                              False
                              (isWinner [Empty 1, Empty 2, Player 'O', Empty 4, Player 'X', Player 'X', Player 'X', Player 'O', Empty 9]))
test7 = TestCase (assertEqual "for (placePiece [Empty 1, Empty 2, Empty 3, Empty 4, Empty 5, Empty 6, Empty 7, Empty 8, Empty 9] (Player 'X') 5 ),"
                              [Empty 1, Empty 2, Empty 3, Empty 4, Player 'X', Empty 6, Empty 7, Empty 8, Empty 9]
                              (placePiece [Empty 1, Empty 2, Empty 3, Empty 4, Empty 5, Empty 6, Empty 7, Empty 8, Empty 9] (Player 'X') 5 ))


tests = TestList [TestLabel "test0" test0 , TestLabel "test1" test1,TestLabel "test2" test2, TestLabel "test3" test3,TestLabel "test4" test4,TestLabel "test5" test5,TestLabel "test6" test6, TestLabel "test7" test7]