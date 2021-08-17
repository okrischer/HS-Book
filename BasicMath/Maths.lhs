\chapter{Basic Maths}
\section{Multiplication}

\begin{code}
module Maths (egyptMult, egyptAncient) where
import Data.List (elem, elemIndex, findIndex)
import Data.Maybe (fromMaybe)
import Criterion.Main
\end{code}

\subsection{Egyptian Multiplication}

The first known algorithm, later occasionally called the russian peasant algorithm, is a procedure for calculating the product of two integer numbers, known as *Egyptian Multiplication*. It relies on the insight that 
\begin{equation*}
n \cdot a = \frac{n}{2} \cdot (a+a), \text{e.g. } 4 \cdot a = 2(a+a).
\end{equation*}
This algorithmn has a complexity of $\mathcal{O}(\lg n)$, because we are halving the input number of *n*, by which *a* is added to itself for each subsequent step of calculation. In contrast, a naive calculation, which would add *a* *n*-times to itself, would have a complexity of $\mathcal{O}(n)$.

\begin{code}
multi :: (Int, Int, Int) -> Int
multi (n, a, r) -- r is the intermediate result for each step
    | n == 1    = r + a
    | odd n     = multi (half_n, a+a, r+a)
    | otherwise = multi (half_n, a+a, r)
    where
        half_n = n `div` 2

egyptMult :: (Int, Int) -> Int
egyptMult (n, a) = multi (n, a, 0)
\end{code}

Of course, this algorithm would have been difficult to calculate in ancient times, as we have to divide by 2 many times, which is not a trivial operation. Furthemore, we want to get rid of checking against *odd* and of the first branch of our algorithm, which handels these cases. One simple solution for this would be to assume that n is always a power of two. Not surprisingly, the ancient Egypts found this solution too, which we can now use to enhance our algorithm: the only aids for this are a lookup-table for powers of 2 and another algorithm for decomposing a number into its powers of 2.

\begin{code}
table :: [Int]
table = [2^x | x <- [0..30]]

decompose :: Int -> [Int]
decompose n
  | n == 1 = [1]
  | n `elem` table = [n]
  | otherwise =  m : decompose p
  where
      m = table !! (o-1)
      Just o = findIndex (> n) table
      p = n - m

multiplicate :: (Int,  Int, Int) -> Int
multiplicate (n, a, o) -- o ist the position of factor n in our lookup
  | n == 1 = a
  | otherwise = multiplicate (m, a+a,  o-1)
  where
      m = table !! (o-1)

egyptAncient :: (Int, Int) -> Int
egyptAncient (n, a) =
  let
    factors = decompose n
    pos = map (`elemIndex` table) factors
    positions = map (fromMaybe 0) pos
    values = zip3 factors (repeat a) positions
  in
    sum(map multiplicate values)
\end{code}

With this, we can mimic the exact way of how the Egyptians used this algorithm. Unfortunately this also slows down our calculation, presumably due to the repeated lookup of values in a list, which obviously isn't the smartest way to construct lookup-tables. We will see examples of better implementations in part II - *Algorithm Design*.

\subsection{Karatsuba Multiplication}
Of course, there are more recent algorithms for multiplication, one of them the *Karatsuba Multiplication*, named after Anatoly Karatsuba, who discovered this recursive algotithm in 1960.

\subsection{Comparing Multiplication Algorithms}

