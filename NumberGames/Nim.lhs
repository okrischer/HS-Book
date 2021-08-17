\chapter{The Game of Nim}

\begin{code}
module Nim (nim, initial) where
import qualified Data.Char as Char
import Data.Bits
\end{code}

\section{The Goal of Nim}
In *nim* you are given a specified number of rows or heaps, each of them containing a specified number of items larger than zero. It is your task to remove one or more items from one row for every move, where two players are in turn alternately. There are two versions of the game: the first, in which the player wins, whoever takes the last item from the board, and the other, in which the same player looses. The gameplay is identical for both variants, so we will stick to the first variant in the course of this example.

\section{Creating the Game Play}

We are going to build the logic for the game *bottom up*, which means in this case that we build all the needed singular-purpose functions at first and then compose the main game logic out of these single functions.

First of all, we need a function to switch players for every move:

\begin{code}
next :: Int -> Int 
next 1 = 2
next 2 = 1
\end{code}

Next, we make some definitions for creating the board and checking the game status:

\begin{code}
type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)
\end{code}

A move is specified by a row number and the number of stars to be removed, and it is valid if the row contains at least this many stars:

\begin{code}
valid :: Board -> (Int, Int) -> Bool
valid board (row, num) = board !! (row-1) >= num

move :: Board -> (Int, Int) -> Board
move board (row, num) = [update r n | (r,n) <- zip [1..] board]
    where update r n = if r == row then n-num else n
\end{code}

Having the basic game mechanics out of the way, we can now define some IO utilities for printing out the game status and accepting user input:

\begin{code}
putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     d <- getChar
                     newline
                     if Char.isDigit d then
                        return (Char.digitToInt d)
                     else 
                        do putStrLn "error: no digit provided!"
                           getDigit prompt

newline :: IO ()
newline = putChar '\n'
\end{code}

Finally, we create the main game loop:

\begin{code}
nimH2H :: Board -> Int -> IO ()
nimH2H board player = do
    newline
    putBoard board
    if finished board then do 
        newline
        putStr "player "
        putStr (show (next player))
        putStrLn " wins!!"
    else do
        newline
        putStr "player "
        print player
        row <- getDigit "enter a row number: "
        num <- getDigit "stars to remove: "
        if valid board (row, num) then
            nimH2H (move board (row, num)) (next player)
        else do
            newline
            putStrLn "error: invalid move!"
            nimH2H board player
\end{code}

With that, we have a first running version of *nim*, which could be played in GHCI with two human players *head to head*, e.g. with `ghci> nimH2H initial 1`, starting with player 1. Of course, this is only fun if you have a friend to play with, as the game is missing any logic by now. We will take care of this in the next section.

\section{Creating the Game Logic}

If you try to win the game we have so far, you will soon recognize that it is not as trivial as it seems at first, as you have to think a few moves in advance and calculate the possible moves of your opponent.

A general winning strategy for a game is to calculate all the remaining possible moves at each turn and to store them in an appropriate data structure, often called the *game tree*. We will lateron see an example for this in the chapter of *Tic Tac Toe*. However, in the case of nim this is not a promising strategy, as nim is a non-deterministic game, for which you cannot compute all remaining moves, at least not in a reasonable time.

Thankfully, there is another way of tackling this problem, which is, however, anything but obvious. To make a long story very short, here is an algorithm for a winning strategy for this kind of games:

1. Compute the `Nim-Sum` of the given board. The `Nim-Sum` is the *bitwise exclusive or* of the numbers of elements in each row: $s_n = r_1 \oplus r_2 \oplus \cdots \oplus r_n$
2. If the `Nim-Sum` is zero make a stalling move, otherwise continue with step 3
3. Compute the *bitwise xor* of the `Nim-Sum` with the number of elements of every given row: $n_1 = s_n \oplus r_1, n_2 = s_n \oplus r_2, \cdots , n_n = s_n \oplus r_n$
4. Find a row, whose result of step 3 is less than its number of elements
5. Reduce the number of elements in the row of step 4, such that it equals its result from step 3.  

The key point of this algorithm is the insight that you can't win the game if the `Nim-Sum` is zero and it's your turn to move. This is why we make a stalling move in step 2 of the algorithm, hoping for a mistake of the human player. To give the human player at least a chance to win, we will let him make the first move with the initial board, whose `Nim-Sum` is not zero, which you are welcome to check.

In the following we give the implematation of the algorithm, combining steps 3 to 5 into one single function:

Compute the `Nim-Sum` for the given board:
\begin{code}
nimSum :: Board -> Int
nimSum = foldr xor 0
\end{code}

Make stalling move:
\begin{code}
stall :: Board -> Int -> (Int, Int)
stall (x:xs) row
    | x > 0 = (row, 1)
    | otherwise = stall xs (row+1)
\end{code}

Reduce the elements of a row to its *bitwise xor* with `Nim-Sum`:
\begin{code}
reduce :: Board -> Int -> (Int, Int)
reduce board row
    | x > bxor = (row, x-bxor)
    | otherwise = reduce board (row+1)
    where x = board !! (row-1)
          bxor = xor (nimSum board) x
\end{code}

Finally, we adjust the present game play to allow the computer (player 2) to make its own choice:
\begin{code}
nim :: Board -> Int -> IO ()
nim board player = do
    newline
    putBoard board
    if finished board then do 
        newline
        if player == 1 then print "computer wins!!"
        else print "you win!!"
    else do
        newline
        if player == 1 then do -- human player
            row <- getDigit "enter a row number: "
            num <- getDigit "stars to remove: "
            if valid board (row, num) then
                nim (move board (row, num)) 2
            else do
                newline
                putStrLn "error: invalid move!"
                nim board player
        else do -- computer player
            if nimSum board == 0 then nim (move board (stall board 1)) 1
            else nim (move board (reduce board 1)) 1
\end{code}
The game can now be startet in GHCI with

    ghci> :load Nim
    ghci> nim initial 1

