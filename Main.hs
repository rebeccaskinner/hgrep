import DirTraverse
import Pager
import System.Directory

main = getCurrentDirectory >>= getFiles >>= mapM_ pageFile
