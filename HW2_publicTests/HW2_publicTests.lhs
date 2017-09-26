Homework 1 (Public Tests)

\begin{code}
module HW2_publicTests where
import HW2 as Stud
import System.Environment
--import TicTacToe.Src.TicTacToe as TTT
import Main as TTT (playRounds, player2)
import Player.MinMax as MM (playerMinMax)
\end{code}

Helper Functions
\begin{code}
sameElems :: Eq a => [a] -> [a] -> Bool 
sameElems xs ys = length xs == length ys && all (`elem` ys) xs

isPrefixOf :: String -> String -> Bool
isPrefixOf s1 s2 = s1 == [x | (x,y) <- zip s1 s2, x == y]
\end{code}

Problem 1

\begin{code}
-- inputs
tree5 = Bin "Hello" Tip (Bin "World" Tip Tip)

-- map outputs
mapOut2 = Bin 2 (Bin 3 Tip Tip) Tip
mapOut3 = Bin 1 mapOut2 mapOut2
mapOut4 = Bin "I am!" (Bin "a!" Tip Tip) (Bin "tree!" Tip Tip)
mapOut5 = Bin "Hello!" Tip (Bin "World!" Tip Tip)

publicMap :: Bool
publicMap =
  Prelude.map (Stud.map (+ 1)) [tree2, tree3] == [mapOut2, mapOut3] &&
  Prelude.map (Stud.map (++"!")) [tree4, tree5] == [mapOut4, mapOut5]

-- fold outputs
foldOut2 = 3
foldOut3 = 6
foldOut4 = "I amatree"
foldOut5 = "HelloWorld"

publicFold :: Bool
publicFold =
  Prelude.map (Stud.fold (+) 0) [tree2, tree3] == [foldOut2, foldOut3] &&
  Prelude.map (Stud.fold (++) "") [tree4, tree5] == [foldOut4, foldOut5]

-- elems outputs
elemsOut2 = [2,1]
elemsOut3 = [2,1,2,1,0]
elemsOut4 = ["tree", "a", "I am"]
elemsOut5 = ["World", "Hello"]

-- Should we care about ordering??
publicElems :: Bool
publicElems = (and $ zipWith (\xs ys -> sameElems xs ys) t1 t2) &&
              (and $ zipWith (\xs ys -> sameElems xs ys) t3 t4)
  where t1 = Prelude.map (Stud.elems) [tree2, tree3]
        t2 = [elemsOut2, elemsOut3]
        t3 = Prelude.map (Stud.elems) [tree4, tree5] 
        t4 = [elemsOut4, elemsOut5]
\end{code}

Problem 2

\begin{code}
-- inputs
l1 = [1..4]
l2 = [1..5]
l3 = [1,11,2]
l4 = [1,3..11]
l5 = [2,4..12]
l6 = [1,5,9]
l7 = [2,8]
l8 = [1,5,2,1,8,7]
l9 = [100,99..1]

-- split outputs
splOut1 = ([1,2], [3,4])
splOut2 = ([1,2], [3,4,5])
splOut3 = ([1], [11, 2])
splOut4 = ([1,3,5], [7,9,11])

publicSplitHalf :: Bool
publicSplitHalf =
  Prelude.map Stud.splitHalf [l1, l2, l3, l4] == [splOut1, splOut2, splOut3, splOut4]

-- merge/mergeBy outputs
mrgOut12 = [1,1,2,2,3,3,4,4,5]
mrgOut45 = [1..12]
mrgOut67 = [1,2,5,8,9]

publicMerge :: Bool
publicMerge =
  Prelude.map (\(xs, ys) -> Stud.merge xs ys) [(l1, l2), (l4, l5), (l6, l7)] ==
  [mrgOut12, mrgOut45, mrgOut67]

publicMergeBy :: Bool
publicMergeBy =
  Prelude.map (\(xs, ys) -> Stud.mergeBy compare xs ys)
  [(l1, l2), (l4, l5), (l6, l7)] == [mrgOut12, mrgOut45, mrgOut67]

-- mergeSortBy outputs
mrgSrtOut3 = [1,2,11]
mrgSrtOut8 = [1,1,2,5,7,8]
mrgSrtOut9 = [1..100]

publicMergeSortBy :: Bool
publicMergeSortBy =
  Prelude.map (Stud.mergeSortBy compare) [l3, l8, l9] ==
  [mrgSrtOut3, mrgSrtOut8, mrgSrtOut9]
\end{code}

Problem 3
\begin{code}
-- l1 = [1..4]
-- l2 = [1..5]
-- l3 = [1,11,2]
-- l4 = [1,3..11]
-- l5 = [2,4..12]
-- l6 = [1,5,9]
-- l7 = [2,8]
-- l8 = [1,5,2,1,8,7]
-- l9 = [100,99..1]
-- inputs 
l10 = [7]

-- mergeAll outputs
mrgAllOut12 = [1,1,2,2,3,3,4,4,5]
mrgAllOut45 = [1..12]
mrgAllOut6710 = [1,2,5,7,8,9]

publicMergeAll :: Bool
publicMergeAll =
  Prelude.map Stud.mergeAll [[l1, l2], [l4, l5], [l6, l7, l10]] ==
  [mrgAllOut12, mrgAllOut45, mrgAllOut6710]

-- descending outputs
descOut0l1 = [[0], [1..4], []]
descOut2l8 = [[1,2], [1,2,5], [7,8], []]
descOut101l9 = [[1..101], []]

publicDescending :: Bool
publicDescending =
  Prelude.map (\(i, l) -> Stud.descending i [] l) [(0, l1), (2,l8), (101, l9)] ==
  [descOut0l1, descOut2l8, descOut101l9]

-- ascending outputs
ascOut0l1 = [[0..4],[]]
ascOut2l8 = [[2],[1,5],[1,2],[7,8], []]
ascOut101l9 = [[101],[1..100], []]

publicAscending :: Bool
publicAscending = 
  Prelude.map (\(i, l) -> Stud.ascending i id l) [(0, l1), (2,l8), (101, l9)] ==
  [ascOut0l1, ascOut2l8, ascOut101l9]
\end{code}

Problem 4

\begin{code}
publicMinMax :: IO Bool
publicMinMax = do
  [(_, _), (_,s)] <- playRounds 5 MM.playerMinMax player2 
  return (s == 0)
\end{code}

Public Test Runner

\begin{code}
runPublicTests :: String -> IO ()
runPublicTests func =
  case func of 
    "publicMap"    ->
      if publicMap then putStrLn "Passed publicMap!"
      else error ("Failed publicRemoveMap.")
    "publicFold"        -> 
      if publicFold then putStrLn "Passed publicFold!"
      else error ("Failed publicFold.")
    "publicElems"     -> 
      if publicElems then putStrLn "Passed publicElems!"
      else error ("Failed publicElems.")
    "publicSplitHalf"        -> 
      if publicSplitHalf then putStrLn "Passed publicSplitHalf!"
      else error ("Failed publicSplitHalf.")
    "publicMerge"        -> 
      if publicMerge then putStrLn "Passed publicMerge!"
      else error ("Failed publicMerge.")
    "publicMergeBy" -> 
      if publicMergeBy then putStrLn "Passed publicMergeBy!"
      else error ("Failed publicMergeBy.")
    "publicMergeSortBy"     -> 
      if publicMergeSortBy then putStrLn "Passed publicMergeSortBy!"
      else error ("Failed publicMergeSortBy.")
    "publicMergeAll" -> 
      if publicMergeAll then putStrLn "Passed publicMergeAll!"
      else error ("Failed publicMergeAll.")
    "publicDescending" -> 
      if publicDescending then putStrLn "Passed publicDescending!"
      else error ("Failed publicDescending.")
    "publicAscending" -> 
      if publicAscending then putStrLn "Passed publicAscending!"
      else error ("Failed publicAscending.")
    "publicMinMax" ->
      publicMinMax >>= (\b ->
                          if b then putStrLn "Passed publicMinMax!"
                          else error ("Failed publicMinMax.")
                       )
    otherwise              -> error ""

main = do
  args <- getArgs
  let func = head args in
    if ("public" `isPrefixOf` func) then runPublicTests func
    else error "What tests are these?"
\end{code}
