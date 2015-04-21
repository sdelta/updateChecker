module FileInteraction (
        Author(..),
        prepareAppDirectory,
        readKnownAuthors,
        urlOrNickToAuthorList,
        downloadAndUpdateCache,
        authorShowComponents,
        saveAuthors) 
    where

import Control.Monad (foldM, when)
import Control.Applicative ((<$>), (<*>))
import Data.Char (toLower)
import Data.Maybe (fromJust, isJust)
import Data.List (elemIndex)
import Data.Either.Utils (forceEither)

import System.FilePath (combine,
                        pathSeparator)

import System.Directory (doesFileExist, 
                         getAppUserDataDirectory, 
                         createDirectoryIfMissing,
                         getHomeDirectory,
                         setCurrentDirectory,
                         renameFile)


import System.IO (readFile, 
                       writeFile)

import Prelude hiding (readFile, 
                       writeFile)

import HTML

data Author = Author {
    authorWebPage :: String,
    authorNick :: String
} deriving (Read, Show)

cacheFolderName = "cache"
appName = "updateChecker"
appDir = "." ++ appName
authorsFile = "known_authors"

downloadAndUpdateCache :: Author -> IO (AuthorPage, AuthorPage)
downloadAndUpdateCache author = 
    let 
        pageToCacheName :: AuthorPage -> String
        pageToCacheName page = cacheFolderName ++ [pathSeparator] ++ authorNick author ++ "." ++ (show $ pageHash page)

        rewriteCache :: String -> AuthorPage -> IO ()
        rewriteCache cachedFile page = writeFile cachedFile $ show page

    in do
        page <- downloadAuthorPage $ authorWebPage author
        let cachedFile = pageToCacheName page
        isCacheExist <- doesFileExist cachedFile
        cachedPage <- if isCacheExist
            then do
                cachedAuthorPage <- (read <$> readFile cachedFile) :: IO AuthorPage
                renameFile cachedFile (cachedFile ++ ".old")
                rewriteCache cachedFile page 
                return cachedAuthorPage
            else do
                rewriteCache cachedFile page 
                return emptyPage 
        return (page, cachedPage)

authorShowComponents :: Author -> [String]
authorShowComponents author = [authorNick author, authorWebPage author]

urlOrNickToAuthorList :: [Author] -> String -> [Author]
urlOrNickToAuthorList knownAuthorsList str = 
    let
        authorsNicks = map authorNick knownAuthorsList
    in
        if str `elem` authorsNicks
        then
            [knownAuthorsList !! fromJust (elemIndex str authorsNicks)]
        else
            if str == "all"
            then
                knownAuthorsList
            else
                [Author str "unknown author"]

prepareAppDirectory :: IO ()
prepareAppDirectory = do
    targetDir <- combine <$> getHomeDirectory <*> return appDir 
    createDirectoryIfMissing False targetDir
    setCurrentDirectory targetDir
    createDirectoryIfMissing False cacheFolderName

readKnownAuthors :: IO [Author]
readKnownAuthors = do
    isFileExist <- doesFileExist authorsFile 
    if isFileExist
    then
        read <$> readFile authorsFile
    else
        return []

saveAuthors :: [Author] -> IO ()
saveAuthors authorsList = writeFile authorsFile $ show authorsList
