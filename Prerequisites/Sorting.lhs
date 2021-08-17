\chapter{Searching and Sorting}

\begin{code}
module Main where
import Criterion.Main
import Data.List (unfoldr, sort)
import System.Random

\end{code}

\section{Searching}
\subsection{Binary Search}

\section{Sorting}
\subsection{Insertion Sort}

Insertion sort has a runtime complexity in the worst case of $\mathcal{O}(n^2)$, as most of the other linear sorting algorithms do, because for every `n` in the input list we have to compare against every other `n`, leading to $(n * n)$ steps of computation.
First we implement the *helper*-function `insert`, which inserts a value at the correct position into an already sorted list:

\begin{code}
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x <= y    = x:y:ys
    | otherwise = y : insert x ys
\end{code}

Second we implement the *master*-function `isort`, which uses `insert` to build a sorted list. The following version uses explicit recursion, which demonstrates perfectly the underlying idea of inserting into an already sorted list:
\begin{code}
isort' :: Ord a => [a] -> [a]
isort' [] = []
isort' (x:xs) = insert x (isort' xs)
\end{code}

The following version of `isort` uses the higher order function `foldr` for implicit recursion, which does exactly the same as the preceeding version:
\begin{code}
isort :: Ord a => [a] -> [a]
isort = foldr insert []
\end{code}

\subsection{Quick-Sort}
Quicksort is a very fast linear sorting algorithm, which introduces the idea of breaking up the list of input values into two independent lists for every next iteration of the recursion. Note that the runtime complexity in the worst case is still $\mathcal{O}(n^2)$, but the average runtime can be reduced to $\mathcal{O}(n \lg(n))$ for an appropriate pivot value of `x`, if `x` splits the current list into two lists of almost equal length.
\begin{code}
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b >  x]
\end{code}

\subsection{Merge-Sort}
Merge sort enhances the idea of breaking up the input list into two independent lists, but in contrast to quicksort it ensures that the created sub-lists have the same length. This is accomplished by splitting up the input list at the middle position, regardless to the actual value. Thus, the input length is halved for every iteration and the runtime complexity for the worst case becomes $\mathcal{O}(n \lg n)$. The actual implementation is done by recursivly sorting the splitted lists and then merging them together into one result list. Like insertion sort this is an example for a *divide and conquer* algorithm, where the main method `msort` uses a helper function `merge` for processing the created sub-lists. For the following implementation we also need a helper function `halve` for splitting up the input list.
\begin{code}
halve :: [a] -> ([a], [a])
halve xs = splitAt n xs
    where n = length xs `div` 2

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge left@(x:xs) right@(y:ys)  | x <= y    = x : merge xs right
                                | otherwise = y : merge left ys

msort :: Ord a => [a] -> [a]
msort []    = []
msort [x]   = [x]
msort xs    = merge (msort left) (msort right)
              where (left, right) = halve xs
\end{code}

\subsection{Conclusion}
As the benchmarking report shows, *insertion sort* is by far the fastest sorting method on lists, beeing at least twice as fast as the buildin function from Data.List. On second thoughts this is not all surprising, for it is very *'cheap'* to insert values into a linked list in contrast to doing so for an array, for which we would have to move every element after the inserted value one place to the *'right'*. Quick-sort is also pretty fast on lists, despite of not beeing optimized regarding its pivot value. However, the merge-sort does not fulfill the expectance of being the fastest linear sorting algorithm, at least not in this implementation on lists.
\begin{code}
getRanList :: RandomGen g => Int -> g -> [Word]
getRanList n = take n . unfoldr (Just . uniformR (1, 100))

main = do {
    let smallInput  = getRanList 100 (mkStdGen 42)
        mediumInput = getRanList 1000 (mkStdGen 42)
        largeInput  = getRanList 10000 (mkStdGen 42)
    in
        defaultMain [
        bgroup "small"  [ bench "sort"   $ whnf sort  smallInput
                        , bench "isort"  $ whnf isort smallInput
                        , bench "qsort"  $ whnf qsort smallInput
                        , bench "msort"  $ whnf msort smallInput
                        ],
        bgroup "medium" [ bench "sort"   $ whnf sort  mediumInput
                        , bench "isort"  $ whnf isort mediumInput
                        , bench "qsort"  $ whnf qsort mediumInput
                        ],
        bgroup "large"  [ bench "sort"   $ whnf sort  largeInput
                        , bench "isort"  $ whnf isort largeInput
                        , bench "qsort"  $ whnf qsort largeInput
                        ]
        ]
}
\end{code}
![bench-sort](img/sort-bench.png)\
