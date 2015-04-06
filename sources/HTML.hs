module HTML(
        downloadPage, 
        getBooksDesc, 
        pageToPageTitleHash, 
        compareBookDesc,
        printPageState,
        compareBookLst,

        BookDescription, 
        PageState(..),
        maxPageStateLength)
    where

import Control.Monad.State
import Control.Applicative ((<$>))
import Data.Hashable (hash)
import Text.HTML.TagSoup as HTML
import Network.HTTP (simpleHTTP, 
                     getRequest, 
                     getResponseBody)

type HTMLPage = [Tag String]

data BookDescription = BookDescription {
    bookTitle :: String,
    bookSize  :: Int,
    bookDescription :: String
} deriving (Read, Show, Eq)

-- in order of precendence of printing (firsts suppress following)
data PageState = UpdatedBooksCount | 
                UpdatedBookTitle | 
                UpdatedBookSize |
                UpdatedBookDesc |
                Inited | 
                Unchanged deriving (Show, Eq, Ord, Bounded, Enum)

printPageState :: PageState -> String
printPageState UpdatedBookTitle = "title changed"
printPageState UpdatedBookSize = "size changed"
printPageState UpdatedBookDesc = "description changed"
printPageState UpdatedBooksCount = "books count changed"
printPageState x = show x


compareBookDesc :: BookDescription -> BookDescription -> PageState
compareBookDesc a b | a == b = Unchanged
compareBookDesc a b | bookTitle a /= bookTitle b = UpdatedBookTitle
compareBookDesc a b | bookSize a /= bookSize b = UpdatedBookSize
compareBookDesc a b = UpdatedBookDesc

compareBookLst :: [BookDescription] -> [BookDescription] -> PageState
compareBookLst a b | length a /= length b = UpdatedBooksCount
compareBookLst a b = minimum $ zipWith compareBookDesc a b

maxPageStateLength :: Int
maxPageStateLength = maximum $ map (length . printPageState) $ enumFrom (minBound :: PageState)

downloadPage :: String -> IO HTMLPage
downloadPage url = do
    rawPage <- simpleHTTP (getRequest url) >>= getResponseBody
    return $ canonicalizeTags $ parseTags rawPage

-- assume that calling dividePageToBookSubPages be after canonicalizeTags function
dividePageToBookSubPages :: HTMLPage -> [HTMLPage]
dividePageToBookSubPages page = filter isBeginingOfDesc $ HTML.sections (~== TagOpen "dl" []) page
    where
        beginingOfDesc :: HTMLPage
        beginingOfDesc = [TagOpen "dl" [], TagOpen "dt" [], TagOpen "li" []]

        isBeginingOfDesc :: HTMLPage -> Bool
        isBeginingOfDesc list = take (length beginingOfDesc) list == beginingOfDesc

pageToBookDescription :: HTMLPage -> BookDescription
pageToBookDescription tagList = 
    let
        extractText :: HTMLPage -> String
        extractText page = concatMap fromTagText $ filter isTagText page
    in
        flip evalState tagList $ do
            modify (tail . dropWhile (~/= "<a>"))
            title <- extractText <$> takeWhile (~/= " &nbsp; ") <$> get
            modify (tail . tail . dropWhile  (~/= " &nbsp; "))
            size <- (read <$> init <$> fromTagText <$> head <$> get) :: State HTMLPage Int
            modify $ dropWhile (~/= "<dd>")
            modify $ takeWhile (~/= "</dl>")
            description <- extractText <$> get
            return $ BookDescription title size description

getBooksDesc :: HTMLPage -> [BookDescription]
getBooksDesc page = map pageToBookDescription $ dividePageToBookSubPages page

-- assume that calling pageTagsToPageTitleHash be after canonicalizeTags function
pageToPageTitleHash :: HTMLPage -> Int
pageToPageTitleHash page = flip evalState page $ do
    modify (tail . dropWhile (~/= "<h3>"))
    modify $ takeWhile (~/= "</h3>")
    pageTitle <- concatMap fromTagText <$> filter isTagText <$> get
    return $ abs $ hash pageTitle
