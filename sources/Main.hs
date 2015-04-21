module Main
    where

import System.Environment (getArgs)
import Control.Monad (when, unless, join)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Reader (ReaderT, Reader, runReaderT, ask, asks)
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import System.Console.Readline (readline)
import System.IO (putStr)
import Data.ConfigFile (ConfigParser)
import Data.List (intersperse)
import Prelude hiding (putStr)

-- internal modules
import FileInteraction
import Reporting
import Printable (Printable)
import DrawReport

data Environment = Environment {
    envArgs :: [String],
    envAuthors :: [Author]
}

askStr :: String -> IO String
askStr question = do
    maybeStr <- readline question
    case maybeStr of
        Nothing -> error "unexpected eof"
        Just str -> return str

checkUpdate :: ReaderT Environment IO ()
checkUpdate = 
    let
        printColoredLst :: Chan Printable -> IO ()
        printColoredLst channel = do
            printable <- readChan channel
            printResultColored printable

        threadTemplate :: Chan Printable -> (IO Printable) -> IO ()
        threadTemplate channel ioPrintable = do
            printable <- ioPrintable
            writeChan channel printable

        processAuthorPage :: Int -> Author -> IO Printable
        processAuthorPage maxNickLen (Author link nick) = do
            (newPage, oldPage) <- downloadAndUpdateCache $ Author link nick
            let shortReport = summarize $ reportDifferencies oldPage newPage
            return $ summaryToPrintable (nick, maxNickLen) shortReport
         
    in do
        arg <- (!! 1) <$> asks envArgs
        knownAuthorsList <- asks envAuthors
        let authors = urlOrNickToAuthorList knownAuthorsList arg
        let maxNickLen = maximum $ map (length . authorNick) authors

        channel <- liftIO $ newChan

        let printableLst = map (processAuthorPage maxNickLen) authors

        threadLst <- liftIO $ sequence $ map (forkIO . threadTemplate channel) printableLst 
        liftIO $ sequence_ $ map (const $ printColoredLst channel) threadLst

listAuthors :: ReaderT Environment IO ()
listAuthors = do 
    matrix <- map (authorShowComponents) <$> asks envAuthors
    liftIO $ printUncoloredMatrix matrix 

addAuthor :: ReaderT Environment IO ()
addAuthor = do
    knownAuthorsList <- asks envAuthors
    nick <- liftIO $ askStr "Enter author's nick: "
    when (nick == "all") $ error "keyword all is reserved"
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
    prepareAppDirectory 
    knownAuthorsList <- readKnownAuthors
    return $ Environment args knownAuthorsList

main = do
    env <- getEnvironment
    case head $ envArgs env of
        "check" -> runReaderT checkUpdate env 
        "list" -> runReaderT listAuthors env
        "add" -> runReaderT addAuthor env
        "delete" -> runReaderT deleteAuthor env
        arg     -> error ("wrong argument: " ++ arg)
