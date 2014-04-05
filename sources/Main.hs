module Main
    where

import System.Environment (getArgs)
import Control.Monad (when, unless, join)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Reader (ReaderT, Reader, runReaderT, ask, asks)
import Control.Applicative ((<$>), (<*>))
import System.Console.Readline (readline)
import System.Console.ArgParser
import System.IO.UTF8 (putStr)
import Data.ConfigFile (ConfigParser)
import Data.List (intersperse)
import Prelude hiding (putStr)

-- internal modules
import FileInteraction
import PrintMatrix
import HTML (maxPageStateLength)

data Command = Check | Add | Delete | List deriving Eq

data CmdLnOptions = CmdLnOptions {
    optCommand :: Command,
    optVerbose :: Bool,
    optAuthorName :: String
}

data Environment = Environment {
    envArgs :: CmdLnOptions,
    envAuthors :: [Author],
    envConfig :: ConfigParser
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
        formatNPrintStr :: [String -> SpecificColor] -> (IO [String], [String]) -> IO()
        formatNPrintStr colorScheme pair = join (printResultColored colorScheme 2 <$> formatStr pair)
            where
                formatStr :: (IO [String], [String]) -> IO [String]
                formatStr (x, y) = joinLists 1 <$> x <*> return y 
    in do
        arg <- asks (optAuthorName . envArgs)
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
    nick <- asks (optAuthorName . envArgs)
    knownAuthorsList <- asks envAuthors
    unless (elem nick $ map authorNick knownAuthorsList) $ error "author with this nick doesn't exist"
    liftIO $ saveAuthors $ filter (\x -> authorNick x /= nick) knownAuthorsList
    
getEnvironment :: IO Environment
getEnvironment = do
    config <- readConfigFile
    prepareAppDirectory config
    knownAuthorsList <- readKnownAuthors
    return $ Environment (CmdLnOptions List False []) knownAuthorsList config 

optionParser :: IO (CmdLnInterface CmdLnOptions) 
optionParser = 
    mkSubParser 
        [ 
            ("check", mkDefaultApp (CmdLnOptions Check `parsedBy` boolFlag "--verbose" `andBy` optPos "all" "author") "check"),
            ("list", mkDefaultApp ((flip (CmdLnOptions List) $ "error") `parsedBy` boolFlag "verbose") "list"),
            ("add", mkDefaultApp (CmdLnOptions Add False `parsedBy` reqPos "author") "add"),
            ("delete", mkDefaultApp (CmdLnOptions Delete False `parsedBy` reqPos "author") "delete")
        ]

setOptions :: Environment -> CmdLnOptions -> Environment
setOptions env opt = Environment opt (envAuthors env) (envConfig env)

chooseFunc :: Environment -> IO ()
chooseFunc env = do
    case optCommand $ envArgs env of
        List -> flip runReaderT env $ listAuthors 
        Add -> flip runReaderT env $ addAuthor
        Delete -> flip runReaderT env $ deleteAuthor
        Check -> flip runReaderT env $ checkUpdate

main :: IO ()
main = do
    env <- getEnvironment 
    interface <- optionParser
    runApp interface (chooseFunc . setOptions env)
