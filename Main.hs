import System.Directory
import System.Environment (getArgs)
import Text.Regex.PCRE
import DirTraverse
import Pager

main = do
    (re:_) <- getArgs
    getCurrentDirectory >>= getFiles >>= (return . filter (\x -> x =~ re :: Bool)) >>= mapM_ pageFile
