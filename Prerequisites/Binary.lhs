\chapter{Binary Numbers}

\begin{code}
module Binary where
import Data.Char (ord, chr)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        "bin2dec":_ -> print(bin2dec "10111001")
        "hex2dec":_ -> print(hex2dec "AFFE")
        "dec2hex":_ -> print(dec2hex 45054)
        [] -> print "Binary Numbers"
\end{code}

\section{The Decimal System}

The primal numeral system used by humans is the *decimal system*, which is a
**positional numeral system** with the base 10. This means that every digit 
$d \in \{0,1,2,3,\ldots,9\}$ of a decimal number has a different value according
to its place in the number. In particular, every digit of the number is
multiplied with a power of ten, where the exponent for the rightmost
(least significant) digit is 0, and the exponent of every following digit
(read from right to left) is increased by 1.

The *decimal* number $1984_{10}$ ist composed of the digits $4,8,9$ and $1$
(reading from right to left), and its value is caluculated like so:
$$1984=4\cdot10^0+8\cdot10^1+9\cdot10^2+1\cdot10^3=4+80+900+1000=1984.$$
Obviously, this calculation doesen't make much sense here, as the value of the
decimal number is already given by itself. But the same rules apply for
calculating the decimal value of a number from any other positional numeral
system. Therefor we just have to substitute the base '10' with the base of
the other numeral system.

The *binary* number $10110_2$ ist composed of the digits $0,1,1,0,1$ 
(reading from right to left, starting with the least significant *bit*). 
Thus, its value is calculated like so:
\begin{equation*}
10110_2=0\cdot2^0+1\cdot2^1+1\cdot2^2+0\cdot2^3+1\cdot2^4=0+2+4+16=22_{10}
\end{equation*}
Trusting for now in that this *algorithm* is valid for any positional numeral
system, we can derive a general formula from it, allowing us to convert a number
with any Base $B$ to a decimal number $Z$:
\begin{equation} \label{eq:sum}
\boxed{Z=\sum_{i=0}^{n-1} Z_i \cdot B^i}
\end{equation}
where $Z,B \in \mathbb{N}$, and $Z$ has $n$ places.

\section{Calculating with Binary Numbers}

Computers store and process information as numbers in the *binary system*,
for which we have already seen a example in the last section. Using only two
different values, called *'binary digits'*, or shorter $bits \in \{0,1\}$, has
proven a very effective way to represent and calculate numbers with a computer.

Starting our efforts in writing efficient haskell code, we will first of all
implement our sum formula 2.1 in haskell.
As the purpose of the following implementation is to outline the idea of the
underlying algorithm, we will not use built-in functions for the conversion
itself.

\begin{code}
type Bit = Char

bin2dec' :: [Bit] -> Int
bin2dec' bits =
    sum [w*b | (w,b) <- zip weights (reverse $ char2dgt bits)]
        where weights = iterate (*2) 1

char2dgt :: [Bit] -> [Int]
char2dgt [] = []
char2dgt (c:cs)
    | c <= '9'  = ord c - 48 : char2dgt cs
    | c <  'A'  = error "not a digit"
    | c <= 'F'  = ord c - 55 : char2dgt cs
    | otherwise = error "not a digit"
\end{code}

There is, however, a simpler way to define `bin2int`, which can be revealed with
the aid of some algebra. Consider an arbitrary four-bit binary number "$b_3b_2b_1b_0$".
Applying the *sum-formula* to this sequence of bits will give
\begin{equation*}
(1 \cdot b_0) + (2 \cdot b_1) + (4 \cdot b_2) + (8 \cdot b_3)
\end{equation*}
which can be restructured by factoring out '2' several times as follows:
\begin{align*}
&b_0+2b_1+4b_2+8b_3 \\
=&b_0+2(b_1+2b_2+4b_3) \\
=&b_0+2(b_1+2(b_2+2b_3)) \\
=&b_0+2(b_1+2(b_2+2(b_3+2 \cdot 0)))
\end{align*}
Now we can generalize the resulting equation by substituting '2' with the base
$B$ of our numeral system and we get a formula known as *Horner's rule*:
\begin{equation} \label{eq:horner}
\boxed{Z=Z_0+B(Z_1+B(Z_2+B(Z_3+\cdots+B(Z_{n-1}+B\cdot0)\cdots)))}
\end{equation}
But what is the benift is of changing a comprehensive formula like the *sum-formula* \eqref{eq:sum} into a cluttered and oblong one like this?

Well, as it turns out, this *recursive* definition is very well suited to being solved with a computer algorithm, especially when implemented in a functional programming language.
Take a look again at *Horner's rule* and try to recognize the recursive
pattern in it:
\begin{equation*}
Z={\color{red}Z_0+B*}({\color{red}Z_1+B*}({\color{red}Z_2+B*}({\color{red}Z_3}+\cdots+B*(Z_{n-1}+B*0)\cdots))).
\end{equation*}
The pattern is: $Z_i + B * X_i$, where $X_i = Z_{i+1} + B * X_{i+1}$.

Let's implement this recursive version of the algorithm in haskell and check, 
if it is holding to its promise of simplicity.

\begin{code}
bin2dec'' :: Int -> [Bit] -> Int
bin2dec'' b bits = 
    foldr (\z x -> z + b * x) 0 (reverse $ char2dgt bits)

bin2dec = bin2dec'' 2
oct2dec = bin2dec'' 8
hex2dec = bin2dec'' 16
\end{code}

\section{Other Binary Systems}

The two other frequently used numeral systems in computer science are the
*octal* and the *hexadecimal* system. One reason for using these bases is that
humans can recognize such numbers much better than long sequences of '0's and
'1's. Another reason is, that these numbers can be very easily converted by hand
from one system into the other, as both share a base that is a power of 2:
the base of *octal* is $2^3=8$, the base of *hexadecimal* is $2^4=16$.

The last step completing our conversions of numeral systems is the conversion
of a decimal number into a number with an arbitrary base. This can be achived by
repeatedly dividing the decimal number by the base of the target system and
taking the remainder, until the decimal number becomes zero:

    13 divided by 2 = 6 remainder 1
     6 divided by 2 = 3 remainder 0
     3 divided by 2 = 1 remainder 1
     3 divided by 2 = 0 remainder 1

The first division results in the least significant bit, so the resulting list
of bits is read from bottom to top and becomes "1101".

\begin{code}
dec2bin' :: Int -> Int -> [Int]
dec2bin' _ 0 = []
dec2bin' b n = n `mod` b : dec2bin' b (n `div` b)

dec2bin n = reverse $ map dgt2char (dec2bin' 2 n)
dec2oct n = reverse $ map dgt2char (dec2bin' 8 n)
dec2hex n = reverse $ map dgt2char (dec2bin' 16 n)

dgt2char :: Int -> Bit
dgt2char d
    | d <= 9  = chr (d + 48)
    | d <= 15 = chr (d + 55)
    | otherwise = error "not a digit"
\end{code}

\section{Representation of Fractions}

Fractions are usually represented as decimal numbers with a decimal point,
seperating the integer places to its left from the decimal place to its right.
For example the fraction $\frac{5}{3}$ could be represented as '1.667' with
one integer and 3 decimal places, rounded to a precision of 4 digits overall.

The rules for calculating the value of a fraction in the decimal
system apply to the binary system: the binary places are calculated as a
power of 2, but this time with a negative exponent, increasing by one and
progressing from the decimal point to the right. So for example, the decimal
value $0.75_{10}$ will be represented as $0.11_{b}$:
\begin{equation*}
0.75=0+1\cdot2^{-1}+1\cdot2^{-2}=\frac{1}{2}+\frac{1}{4}=\frac{3}{4}=0.75
\end{equation*}
So we can generalize our sum formula \eqref{eq:sum} to the following:
\begin{equation}
\boxed{Z=\sum_{i=-m}^{n-1} Z_i \cdot B^i}
\end{equation}
where $Z \in \mathbb{Q}$, $B \in \mathbb{N}$, \
and $Z$ has $n$ iteger and $m$ decimal places.
