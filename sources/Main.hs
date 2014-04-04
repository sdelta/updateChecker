module Main
    where

import System.Environment (getArgs)
import Control.Monad (when, unless, join)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Reader (ReaderT, Reader, runReaderT, ask, asks)
import Control.Applicative ((<$>), (<*>))
import System.Console.Readline (readline)
import System.IO.UTF8 (putStr)
import Data.ConfigFile (ConfigParser)
import Data.List (intersperse)
import Prelude hiding (putStr)

-- internal modules
import FileInteraction
import PrintMatrix
import HTML (maxPageStateLength)

data Environment = Environment {
    envArgs :: [String],
    envAuthors :: [Author],
    envConfig :: ConfigParser
}

askStr :: String -> IO String
askStr question = do
--    putStrLn question
--    getLine
    maybeStr <- readline question
    case maybeStr of
        Nothing -> error "unexpected eof"
        Just str -> return str

checkUpdate :: ReaderT Environment IO ()
checkUpdate = 
    let
        formatNPrintStr :: [String -> SpecificColor] -> (IO [String], [String]) -> IO()
        formatNPrintStr colorScheme pair = join (printResultColored colorScheme 2 <$> formatStr pair)
            where
                formatStr :: (IO [String], [String]) -> IO [String]
                formatStr (x, y) = joinLists 1 <$> x <*> return y 
    in do
        arg <- (!! 1) <$> asks envArgs
        knownAuthorsList <- asks envAuthors
        let authors = urlOrNickToAuthorList knownAuthorsList arg
        let lst = map processAuthorPage authors
        let ioMatrix = map (\x -> sequence [fst x]) lst
        let ioAlignedMatrix = map (alignFields [maxPageStateLength] <$>) ioMatrix
        let pureMatrix = alignStrings' $ map (intersperse " " . snd) lst
        let colorScheme = colorPageState : repeat colorDefault
        liftIO $ mapM_ (formatNPrintStr colorScheme) $ zip ioAlignedMatrix pureMatrix

listAuthors :: ReaderT Environment IO ()
listAuthors = do 
    resultMatrix <- alignStrings' <$> map (intersperse " " . authorShowComponents) <$> asks envAuthors
    liftIO $ mapM_ (printResultColored (repeat colorDefault) 2) resultMatrix

addAuthor :: ReaderT Environment IO ()
addAuthor = do
    knownAuthorsList <- asks envAuthors
    nick <- liftIO $ askStr "Enter author's nick: "
    when (nick == "all") $ error "keyword 'all' is reserved"
    when (elem nick $ map authorNick knownAuthorsList) $ error "author with this nick already exists"
    url <- liftIO $ askStr "Enter author's page: "
    when (elem url $ map authorWebPage knownAuthorsList) $ error "author with this page already exists"    
    liftIO $ saveAuthors $ knownAuthorsList ++ [Author url nick]

deleteAuthor :: ReaderT Environment IO ()
deleteAuthor = do
    nick <- (!! 1) <$> envArgs <$> ask
    knownAuthorsList <- asks envAuthors
    unless (elem nick $ map authorNick knownAuthorsList) $ error "author with this nick doesn't exist"
    liftIO $ saveAuthors $ filter (\x -> authorNick x /= nick) knownAuthorsList
    
getEnvironment :: IO Environment
getEnvironment = do
    args <- getArgs
    when (null args) (error "missing parameters")

    config <- readConfigFile
    prepareAppDirectory config
    knownAuthorsList <- readKnownAuthors
    return $ Environment args knownAuthorsList config 

main = do
    env <- getEnvironment
    case head $ envArgs env of
        "check" -> runReaderT checkUpdate env 
        "list" -> runReaderT listAuthors env
        "add" -> runReaderT addAuthor env
        "delete" -> runReaderT deleteAuthor env
        arg     -> error ("wrong argument: " ++ arg)
