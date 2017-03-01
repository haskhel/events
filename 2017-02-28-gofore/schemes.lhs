\documentclass{beamer}
\usetheme{default}

%% Fonts
\usefonttheme{serif}
\usepackage{mathpazo}
\usepackage{helvet}
\usepackage{courier}
\usepackage{inconsolata}
\usepackage{microtype}

%% Colors
\setbeamercolor{structure}{fg=futugreen}
\definecolor{futugreen}{HTML}{266826}

%% Some packages
\usepackage{hyperref}

%% lhs2TeX
%include polycode.fmt
%include forall.fmt
%format s_0 = "\Varid{s}_0 "
%format s_1 = "\Varid{s}_1 "
%format s_2 = "\Varid{s}_2 "
%format x_1 = "\Varid{x}_1 "
%format x_2 = "\Varid{x}_2 "
%format ... = "\ldots "
%format subst_2 = "\Varid{subst}_2"
%format >>>= = ">\mskip-8mu>\mskip-8mu>\mskip-8mu>\mskip-7mu= "

\newcommand{\at}{\makeatletter |@|\makeatother}

\title{Recursion schemes}
\subtitle{HaskHEL meetup at Gofore}
\author{Oleg Grenrus}
\institute{Futurice}
\date{2017-02-28}

\begin{document}

%if 0
\begin{code}
-- stack --resolver=nightly-2017-02-01 ghci schemes.lhs
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
import Control.Monad (ap, void)
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Debug.Trace
import Numeric.Natural
import Test.QuickCheck
\end{code}
%endif

\begin{frame}
\titlepage
\end{frame}

\begin{frame}
\frametitle{HaskHEL}
\begin{itemize}
\item \texttt{/join \#haskhel} on freenode
\item We need talks: topics, presenters\ldots
\item \url{https://github.com/haskhel/events}
\end{itemize}
\end{frame}


\begin{frame}
\frametitle{References}
\begin{itemize}
\item Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire (1991)
\item Data types à la carte (2008)
\item \href{http://hackage.haskell.org/package/recursion-schemes}{\texttt{recursion-schemes} package}
\end{itemize}
\end{frame}


\begin{frame}
this is literal haskell file
\begin{verbatim}
stack --resolver=nightly-2017-02-01 ghci
Prelude> :l schemes.lhs
\end{verbatim}
to play around.
\end{frame}

\begin{frame}
\frametitle{Factorial}
\begin{code}
data NatF a = ZeroF | SuccF a deriving (Show, Functor)
type instance Base Natural = NatF

instance Recursive Natural where
  project 0  = ZeroF
  project n  = SuccF (n - 1)

factorial :: Natural -> Natural
factorial = para alg where
  alg ZeroF           = 1
  alg (SuccF (n, m))  = (1 + n) * m
\end{code}

%if 0
\begin{code}
factorial_prop :: Natural -> Property
factorial_prop n = factorial n === product [1..n]
\end{code}
%endif
\end{frame}


\begin{frame}
\frametitle{Why not explict recursion?}
\begin{code}
factorial_1 :: Natural -> Natural
factorial_1 0 = 1
factorial_1 n = n * factorial (n - 1)
\end{code}
\end{frame}


\begin{frame}
\frametitle{}
\begin{code}
factorial_2 :: Natural -> Natural
factorial_2 n = product [1..n]
\end{code}

\vspace{2em}
For more see \href{http://www.cs.utexas.edu/~cannata/cs345/Class\%20Notes/10\%20Haskell\%20Programmer\%20Evolution.html}%
{\emph{The Evolution of a Haskell Programmer}}
\end{frame}


\begin{frame}
\frametitle{Spot a bug}
\begin{code}
newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  m >>= k  = State $ \s_0 ->
    let  (x_1, s_1) = runState m s_0
         (x_2, s_2) = runState (k x_1) s_0 -- should be |s_1|
    in (x_2, s_2)
\end{code}
type-checker cannot help here\ldots
%if 0
\begin{code}
instance Functor (State s) where
  fmap f x = x >>= return . f

instance Applicative (State s) where
  pure = return
  (<*>) = ap
\end{code}
%endif
\end{frame}



\begin{frame}
\frametitle{Spot another bug}
\begin{code}
type VarName = String
data Expr  =  Var VarName
           |  Lit Int
           |  Add Expr Expr
           |  Mul Expr Expr
  deriving (Show)

subst :: VarName -> Int -> Expr -> Expr
subst n x (Var n')
  | n == n'          = Lit x
  | otherwise        = Var n'
subst n x (Lit l)    = Lit l
subst n x (Add a b)  = Add (subst n x a) (subst n x b)
subst n x (Mul a b)  = Mul (subst n x a) b  -- no |subst|
\end{code}
\end{frame}


\begin{frame}
In our code base atm
\begin{verbatim}
% git grep 'cata' | wc -l
3
\end{verbatim}
Why? You \emph{had} to write boilerplate.
\end{frame}


\begin{frame}
\frametitle{Outsource writing of boilerplate}
With \emph{TemplateHaskell}
\begin{spec}
import Data.Functor.Foldable.TH
\end{spec}
a single magic spell
\begin{code}
makeBaseFunctor ''Expr
\end{code}
generates
\begin{spec}
data ExprF = ...
type instance Base Expr = ExprF
instance Recursive Expr where project = ...
instance  Corecursive Expr where embed = ...
\end{spec}
%if 0
\begin{code}
deriving instance Show a => Show (ExprF a)
\end{code}
%endif
\end{frame}


\begin{frame}
\begin{code}
subst_2 :: VarName -> Int -> Expr -> Expr
subst_2 n x = cata alg where
  alg (VarF n') | n == n'    = Lit x
  alg x                      = embed x
\end{code}
The following identity holds:
\begin{spec}
cata embed = id
\end{spec}
%if 0
\begin{code}
expr :: Expr
expr = Mul (Add (Var "x") (Lit 1)) (Add (Lit 1) (Mul (Var "x") (Var "y")))
\end{code}
*Main> subst' "x" 42 
Mul (Add (Lit 42) (Lit 1)) (Add (Lit 1) (Mul (Lit 42) (Var "y")))
*Main> subst "x" 42 (Mul (Add (Var "x") (Lit 1)) (Add (Lit 1) (Mul (Var "x") (Var "y"))))
Mul (Add (Lit 42) (Lit 1)) (Add (Lit 1) (Mul (Var "x") (Var "y")))
%endif
\end{frame}

%% Begin decomposing
\begin{frame}
\frametitle{Decomposing cata}
\begin{spec}
data ListF a b = NilF | ConsF a b

cata  ::  (ListF a b       -> b)     -> [a] -> b
\end{spec}
\end{frame}

\begin{frame}
\frametitle{Decomposing cata: 2}
\begin{spec}
data ListF a b = NilF | ConsF a b

cata  ::  (ListF a b       -> b)     -> [a] -> b
cata  ::  (Either () (a,b) -> b)     -> [a] -> b
\end{spec}
\end{frame}

\begin{frame}
\frametitle{Decomposing cata: 3}
\begin{spec}
data ListF a b = NilF | ConsF a b

cata  ::  (ListF a b       -> b)     -> [a] -> b
cata  ::  (Either () (a,b) -> b)     -> [a] -> b
cata  ::  (() -> b) -> ((a,b) -> b)  -> [a] -> b
\end{spec}
\end{frame}

\begin{frame}
\frametitle{Decomposing cata: 4}
\begin{spec}
data ListF a b = NilF | ConsF a b

cata  ::  (ListF a b       -> b)     -> [a] -> b
cata  ::  (Either () (a,b) -> b)     -> [a] -> b
cata  ::  (() -> b) -> ((a,b) -> b)  -> [a] -> b
cata  ::  b -> (a -> b -> b)         -> [a] -> b
\end{spec}
\end{frame}


\begin{frame}
\frametitle{Decomposing cata: 5}
\begin{spec}
data ListF a b = NilF | ConsF a b

cata  ::  (ListF a b       -> b)     -> [a] -> b
cata  ::  (Either () (a,b) -> b)     -> [a] -> b
cata  ::  (() -> b) -> ((a,b) -> b)  -> [a] -> b
cata  ::  b -> (a -> b -> b)         -> [a] -> b
cata  ::  (a -> b -> b) -> b         -> [a] -> b
\end{spec}
\end{frame}

\begin{frame}
\frametitle{Decomposing cata: 6}
\begin{spec}
data ListF a b = NilF | ConsF a b

cata  ::  (ListF a b       -> b)     -> [a] -> b
cata  ::  (Either () (a,b) -> b)     -> [a] -> b
cata  ::  (() -> b) -> ((a,b) -> b)  -> [a] -> b
cata  ::  b -> (a -> b -> b)         -> [a] -> b
cata  ::  (a -> b -> b) -> b         -> [a] -> b

cata = foldr
\end{spec}
\end{frame}

%% END Decomposing

\begin{frame}
\frametitle{Another way to look on cata}
\begin{spec}
cata f = c where c = f . fmap c . project

list :: List Int
list = Cons 1 (Cons 2 (Cons 3 Nil)
\end{spec}
So what |cata| does?

\begin{spec}
cata alg list =
  = c $ (Cons 1 (Cons 2 (Cons 3 Nil)))
  = alg . fmap c . project $ (Cons 1 (Cons 2 (Cons 3 Nil)))
  = alg . fmap c $ ConsF 1 (Cons 2 (Cons 3 Nil))
  = alg $ ConsF 1 $ c (Cons 2 (Cons 3 Nil))
  = alg $ ConsF 1 $ alg . fmap c . project $ (Cons 2 (Cons 3 Nil))
\end{spec}
\end{frame}


\begin{frame}
\begin{spec}
cata alg list =
  ...
  = alg $ ConsF 1 $ alg . fmap c . project $ (Cons 2 (Cons 3 Nil))
  = alg $ ConsF 1 $ alg $ ConsF 2 $ c (Cons 3 Nil)
  = alg $ ConsF 1 $ alg $ ConsF 2 $ alg $ ConsF 3 $ c Nil
  = alg $ ConsF 1 $ alg $ ConsF 2 $ alg $ ConsF 3 $ alg NilF
\end{spec}
so similarly as |foldr f z| replaces list constuctors with |f| and |z|,
|cata| replaces them with |alg (ConsF _ _)| and |alg NilF|
\end{frame}


\begin{frame}
\frametitle{-morphisms}
\begin{center}
\begin{tabular}{lll}
|cata| & fold \\
|para| & also fold \\
|ana|  & unfold \\
|apo|  & also unfold \\
|hylo| & refold \\
\end{tabular}
\end{center}
\end{frame}


\begin{frame}
\frametitle{Monadic morphisms}
\savecolumns
\begin{spec}
cata  ::  Recursive t
      =>  (Base t a    -> a)    -> t -> a
\end{spec}
Monadic variant is not yet in \texttt{recursion-schemes}:
\restorecolumns
\begin{code}
cataM
     ::  (Recursive t, Traversable (Base t), Monad m)
     =>  (Base t a     -> m a)  -> t -> m a
cataM f = (>>= f) . cata (traverse (>>= f))
\end{code}
We can print all intermediate structs:
\begin{code}
ex_1 :: IO ()
ex_1 = cataM print expr
\end{code}
\end{frame}


\begin{frame}
\frametitle{Elm can cata too}
Recursion schemes is advanced technology \ldots of '90s.
\begin{spec}
type ListF a b = NilF | ConsF a b

listCata : (ListF a b -> b) -> List a -> b 
listCata alg l = case l of
  []         -> alg NilF
  (x :: xs)  -> alg (ConsF x (listCata alg xs))
\end{spec}

\emph{Scala} can too: \href{https://github.com/slamdata/matryoshka}{\texttt{matryoshka}}.
\end{frame}


\begin{frame}
\frametitle{aeson-extra}
Uses base functor to restrict the possible choices:
\begin{spec}
merge
    :: (forall a. (a -> a -> a) -> ValueF a -> ValueF a -> ValueF a)
    -> Value -> Value -> Value
\end{spec}
from \href{http://hackage.haskell.org/package/aeson-extra-0.4.0.0/docs/Data-Aeson-Extra-Merge.html}{|Data.Aeson.Extra.Merge| module}
\end{frame}


\begin{frame}
\frametitle{unification-fd}
Uses base functor to amend the recursive type with additional construct:
\begin{spec}
data UTerm t v  =  UVar  v               
                |  UTerm (t (UTerm t v))
\end{spec}
compare to
\begin{spec}
newtype Fix f = Fix (f (Fix f))
\end{spec}
see \href{http://hackage.haskell.org/package/unification-fd-0.10.0.1/docs/Control-Unification.html}{|Control.Unification| module}
\end{frame}


\begin{frame}
\begin{center}
{\Large live long and recurse!}
\end{center}
\end{frame}


\begin{frame}
\frametitle{Extras: IxState}
\begin{code}
newtype IxState i o a = IxState { runIxState :: i -> (a, o) }

infixl 1 >>>=
(>>>=) :: IxState s_0 s_1 a -> (a -> IxState s_1 s_2 b) -> IxState s_0 s_2 b
m >>>= k = IxState $ \s_0 ->
  let  (x_1, s_1) = runIxState m s_0
       (x_2, s_2) = runIxState (k x_1) s_1
  in (x_2, s_2)

bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = to (from m >>>= from . k) where
  to    = State . runIxState
  from  = IxState . runState
\end{code}
\end{frame}

\end{document}
