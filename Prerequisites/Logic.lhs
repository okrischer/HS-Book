\chapter{Mathematical Logic}

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
module Logic where
\end{code}

\section{Logical Connectives}
\subsection{Implication}

Truth table for **implication**: $P \implies Q$ (read as: *'if P, then Q* or *'P implies Q'*):

\begin{center}
\begin{tabular}{c c c}
P & Q & $P \implies Q$ \\
  \hline
  T & T & T \\
  T & F & F \\
  F & T & T \\
  F & F & T \\
  \hline
\end{tabular}
\end{center}

Surprisingly an *implication* with a false proposition is always true, that means one can derive any conclusion from a false proposition (\emph{ex falso quod libet}). Having this in mind, we can give a proper definition of its implementation in Haskell:

\begin{code}
infix 1 ==>
(==>) :: Bool -> Bool -> Bool
p ==> q = not p || q
\end{code}

**Proof for this definition**:
if p is false, the expression is always true. If p is true, the expression is true, when q is true. This corrresponds to the given truth table for *implication*.

\subsection{Equivalence}

The expression $P \iff Q$ is called the **equivalence** of P and Q and is defined as the *conjunction* of the two *implications* $P \implies Q$ and $Q \implies P$. 
\begin{equation} \label{eq:equivalence}
(P \iff Q) \equiv (P \implies Q) \land (Q \implies P)
\end{equation}
From this we can deduce that $P \iff Q$ is true, if P and Q have the same truth value. In other words: if P and Q have the same truth value, they are eqivalent.

**Let's proof this with a truth table**:
\begin{center}
\begin{tabular}{c c c c c}
$P$ & $Q$ & $P \implies Q$ & $Q \implies P$ & $(P \implies Q) \land (Q \implies P)$ \\
  \hline
  T & T & T & T & T \\
  T & F & F & T & F \\
  F & T & T & F & F \\
  F & F & T & T & T \\
  \hline
\end{tabular}
\end{center}

Hence the definition of *equivalence* in Haskell ist trivial:
\begin{code}
infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
p <=> q = p == q
\end{code}

\subsection{Exclusive Or}

**Exclusive or** is a special case of *disjunction*, and evaluates to true, if only one of the conditions is true. Using the insights from *equivalence* we can derive that *exclusive or* is synonym to the negation of *equivalence*:
\begin{equation} \label{eq:xor}
(P \oplus Q) \equiv \lnot (P \iff Q)
\end{equation}

**Proof**: *equivalence* states that P and Q have the same truth value; the negation of this is, that P and Q have different truth values. This is only the case, when one of the members is true and the other false, which is in turn the definition of *exclusive or*. Hence we can define *exclusive or* in Haskell like this:

\begin{code}
infixr 2 <+>
(<+>) :: Bool -> Bool -> Bool
p <+>q = p /= q
\end{code}

\section{Logical Validity}

In order to proof compound logical statements like formulas \eqref{eq:equivalence} and \eqref{eq:xor} with haskell, we will treat such statements as *boolean functions*, taking two or more boolean arguments (representing the involved propositions), and returning the resulting boolean value by applying the involved logical connectives:

\begin{code}
equiFun :: Bool -> Bool -> Bool
equiFun p q = (p <=> q) <=> (p ==> q) && (q ==> p)

xorFun :: Bool -> Bool -> Bool
xorFun p q = (p <+> q) <=> not (p <=> q)
\end{code}

Now we can write a validation function, which takes these boolean functions as its argument and tests against all permutations of truth values for the involved propositions:

\begin{code}
isValid :: (Bool -> Bool -> Bool) -> Bool
isValid bf = and [bf p q | p <- [True, False], q <- [True, False]]
\end{code}

    *Logic> isValid equiFun
    True
    *Logic> isValid xorFun
    True

Istead of checking a single formula for validity, we now can expand this concept for proving that two compound statements are *logical equivalent*, i.e. $\Phi \equiv \Psi$:

\begin{code}
isLogEqui :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool 
isLogEqui bf1 bf2 = 
    and [bf1 p q <=> bf2 p q | p <- [True, False], q <- [True, False]]
\end{code}

With that, we can now show that the following equation, known as the **First Law of De Morgan** is valid:
\begin{equation} \label{eq:demorg1}
\boxed{\neg (P \land Q) \equiv (\neg P \lor \neg Q)}.
\end{equation}

\begin{code}
deMorg1a :: Bool -> Bool -> Bool
deMorg1a p q = not (p && q)

deMorg1b :: Bool -> Bool -> Bool
deMorg1b p q = not p || not q
\end{code}

    *Logic> isLogEqui deMorg1a deMorg1b
    True

\subsection{Working with Type Classes}

In this section we will create a more elegant and idiomatic haskell solution for checking logical validity, based on haskell type classes.
 
\begin{code}
class TF p where
    valid :: p -> Bool
    lequi :: p -> p -> Bool

instance TF Bool where
    valid = id
    lequi f g = f == g
\end{code}

The first definition here creates a new *type class* `TF` (truth functions) and states that every instance of this class must define two functions `valid` and `lequi` with the given signatures. The second definition adds the build-in type `Bool` to the `TF` *type class* and defines the additionally required functions.

The following step creates a new instance of `TF` for the truth functions that we want to test. This is done recursively, in which the instance functions call themselfs, providing the required function argument and a literal `Bool` value as base case for the recursion. This leads to repetetive calls of these functions, which will eventually reduce all the contained propositions to literal `Bool` values and evaluate these by applying the given logic operators (logical connectives). Note, that for this feature to work, we need to enable the haskell language extension `FlexibleInstances`, which allows definition of type class instances with arbitrary nested types.

\begin{code}
instance TF p => TF (Bool -> p) where
    valid f = valid (f True) && valid (f False)
    lequi f g = f True `lequi` g True &&
                f False `lequi` g False
\end{code}

With that, we can now express compound logical statements in terms of instances of `TF`, using *lambda abstraction* for providing the concrete function definition (using again *de Morgan's first law* as an example:
\begin{equation*}
\neg (P \land Q) \equiv (\neg P \lor \neg Q).
\end{equation*}

\begin{code}
deMorg = lequi (\ p q -> not(p && q)) (\ p q -> not p || not q)
\end{code}

    *Logic> dMorg
    True 

