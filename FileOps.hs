module FileOps where
import Data.List.Split
import Utils

hardWrapLines n s = concatMap (chunk n) $ wordsBy (=='\n') s
hardWrap n s = joinByElem '\n' $ hardWrapLines n s
