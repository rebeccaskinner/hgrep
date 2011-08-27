module Utils where

joinByList :: [a] -> [[a]] -> [a]
joinByList _  (x:[]) = x
joinByList jc (x:xs) = x ++ jc ++ joinByList jc xs

joinByElem :: a -> [[a]] -> [a]
joinByElem _  (x:[]) = x
joinByElem jc (x:xs) = x ++ [jc] ++ joinByElem jc xs
