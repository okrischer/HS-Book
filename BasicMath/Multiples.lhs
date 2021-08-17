\section{Project Euler}
In this section we will work through the first two problems of **Project Euler**, which could be found online at [https://projecteuler.net](https://projecteuler.net). There are over 700 mathematical and computational problems available, waiting for your solution. I highly recommend working through some of them in order to train your problem solving abilities. Notice however, that you are in general not allowed to make your solutions public.

\subsection{Problem 1}
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
**Find the sum of all the multiples of 3 or 5 below 1000!**

\subsubsection{Naive solution}

\begin{code}
sumMultiplesNaive :: Integer -> Integer
sumMultiplesNaive n = 
    sum [x | x <- [1..n - 1], x `rem` 3 == 0 || x `rem` 5 == 0]
\end{code}

\subsubsection{Improved solution}
The starting point for developing an efficient solution is the following idea:
instead of checking if the target value is divisible by 3 and 5, we can check separately for division of 3 and 5 and add the results. But then we have to subtract the sum of numbers divisible by `15 (= 3 * 5)`, as we have counted them twice in the first step. When we define a function `sumDivisibleBy :: Int -> Int -> Int`, we can express the result like so:

\begin{code}
sumMultiplesOptim :: Integer -> Integer
sumMultiplesOptim n = 
  sumDivisibleBy3 n + 
  sumDivisibleBy5 n - 
  sumDivisibleBy15 n
\end{code}

If we apply our naive implementation on `sumDivisibleBy` for 3 and 5 we would then get:
\begin{align*}
3+6+9+12+\cdots+999 = 3*(1+2+3+4+\cdots+333) \\
5+10+15+\cdots+995 = 5*(1+2+3+\cdots+199)
\end{align*}

Thus, we can apply the equation for *Triangular Numbers*
\begin{equation*}
T_n = \sum_{k=1}^n k = 1+2+3+\cdots+n = \frac{n*(n+1)}{2}
\end{equation*}

on our function and we get:
\begin{code}
sumDivisibleBy :: Integer -> Integer -> Integer
sumDivisibleBy denom limit = 
  let n = (limit - 1) `div` denom
  in denom * (n * (n + 1)) `div` 2

sumDivisibleBy3 = sumDivisibleBy 3
sumDivisibleBy5 = sumDivisibleBy 5
sumDivisibleBy15 = sumDivisibleBy 15
\end{code}

\subsubsection{Complexity analysis}

The naive implementation has a runtime complexity of $\mathcal{O}(n)$, as the algorithm has to iterate through the entire list of numbers from 1 up to n. So, with growing n, the runtime ist growing proportional to n (i.e. if n is doubled, the runtime will double as well). The optimized version uses a closed formula, which is independent of the growth of n. Thus, the complexity here is constant, or $\mathcal{O}(1)$.

~~~haskell
import Criterion.Main
main = defaultMain [ 
  bgroup "naive"
      [ bench "100"  $ whnf sumMultiplesNaive 100 ],
  bgroup "optim"
      [ bench "1000"  $ whnf sumMultiplesOptim 1000
      , bench "1000000"  $ whnf sumMultiplesOptim 1000000
      , bench "1000000000"  $ whnf sumMultiplesOptim 1000000000 ]
  ]
~~~

![bench-mult](img/bench-mult.png)\
