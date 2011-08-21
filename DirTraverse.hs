module DirTraverse where

import System.IO
import Control.Monad
import System.Directory
import System.FilePath
import Data.List
import Data.List.Split

listDirs =
    filterM doesDirectoryExist

listFiles =
    filterM doesFileExist

fetchContents f d =
    fmap stripHidden (getDirectoryContentsFullPath d) >>= f

fetchDirs =
    fetchContents listDirs

fetchFiles =
    fetchContents listFiles

joinPaths p1 p2 =
    joinPath [p1,p2]

baseName =
    last . wordsBy (=='/')

isHidden =
    isPrefixOf "." . baseName

stripHidden =
    filter (not . isHidden)

getDirectoryContentsFullPath d = do
    contents <- getDirectoryContents d
    return $ map (joinPaths d) contents

getFiles d = do
    files <- fetchFiles d
    dirs <- fetchDirs d
    subFiles <- liftM concat $ mapM getFiles dirs
    return $ files ++ subFiles

-- main = getCurrentDirectory >>= getFiles >>= mapM_ putStrLn
