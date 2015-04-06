module FileInteraction (
        Author(..),
        readConfigFile,
        prepareAppDirectory,
        readKnownAuthors,
        urlOrNickToAuthorList,
        processAuthorPage,
        authorShowComponents,
        saveAuthors) 
    where

import Control.Monad (foldM, when)
import Control.Applicative ((<$>), (<*>))
import Data.ConfigFile
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
                         setCurrentDirectory)


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
configFileName = "." ++ appName ++ ".conf"
authorsFile = "known_authors"

checkAndUpdateCache :: Author -> IO (PageState, String)
checkAndUpdateCache author = 
    let 
        hashToCacheName h = cacheFolderName ++ [pathSeparator] ++ authorNick author ++ "." ++ show h

        rewriteCache :: String -> [BookDescription] -> IO ()
        rewriteCache cachedFile booksDesc = writeFile cachedFile $ show booksDesc

    in do
        page <- downloadPage $ authorWebPage author
        let cachedFile = hashToCacheName $ pageToPageTitleHash page
        isCacheExist <- doesFileExist cachedFile
        let newBooksDesc = getBooksDesc page
        if isCacheExist
        then do
            cachedBooksDesc <- (read <$> readFile cachedFile) :: IO [BookDescription]
            let (state, index) = compareBookLst cachedBooksDesc newBooksDesc
            let url = authorWebPage author ++ getBookLinkByIndex newBooksDesc "" index
            when (state /= Unchanged) $ rewriteCache cachedFile newBooksDesc
            return (state, url)
        else do
            rewriteCache cachedFile newBooksDesc
            return (Inited, authorWebPage author)

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

processAuthorPage :: Author -> IO [String]
processAuthorPage author = do
    (state, url) <- checkAndUpdateCache author
    return [printPageState state, authorNick author, url]

readConfigFile :: IO ConfigParser 
readConfigFile = 
    let
        getDefaultOptionsValues :: IO [(OptionSpec, String)]
        getDefaultOptionsValues = do
            appFolder <- getAppUserDataDirectory appName 
            return [("app_folder_location", appFolder)]

        createConfigParser :: [(OptionSpec, String)] -> ConfigParser
        createConfigParser optionsList = forceEither $ foldM foldFunction emptyCP optionsList
            where
                foldFunction ::  ConfigParser -> (OptionSpec, String) -> Either CPError ConfigParser
                foldFunction configParser (option, value) = set configParser "DEFAULT" (map toLower option) value
    in do
        configFileLocation <- combine <$> getHomeDirectory <*> return configFileName
        isConfigFileExist <- doesFileExist configFileLocation
        defaultConfigParser <- createConfigParser <$> getDefaultOptionsValues
        if isConfigFileExist
        then do
            newConfigParser <- forceEither <$> readfile emptyCP configFileLocation
            return $ merge defaultConfigParser newConfigParser
        else
            return defaultConfigParser

prepareAppDirectory :: ConfigParser -> IO ()
prepareAppDirectory configParser = do
    let targetDir = forceEither $ get configParser "DEFAULT" $ map toLower "app_folder_location"
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
