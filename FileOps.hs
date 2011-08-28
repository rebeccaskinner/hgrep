module FileOps where
import Data.List.Split
import Utils

-- Break the string 's' into lines that are at most 'n' characters wide, then
-- return a list of lines
hardWrapLines n s = concatMap (chunk n) $ wordsBy (=='\n') s

-- Get a version of string 's' wrapped to 'n' characters using '\n' as the
-- newline character
hardWrap n s = joinByElem '\n' $ hardWrapLines n s

-- Get a page 'n' from the text, using a terminal that is 'x' characters
-- wide by 'y' characters high from the string 's'.
getPage x y n s =
    take y $ drop (min (n*(y-1)) ((length s) - y)) $ hardWrapLines x s
