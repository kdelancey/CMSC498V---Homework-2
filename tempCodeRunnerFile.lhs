\begin{code}
data Tree a = Tip | Bin a (Tree a) (Tree a)
  deriving (Eq, Show)
\end{code}  