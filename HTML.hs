module HTML(
        downloadPage, 
        getBooksDesc, 
        pageToPageTitleHash, 
        BookDescription, 
        PageState(..),
        maxPageStateLength)
    where

import Control.Monad.State
import Codec.Binary.UTF8.String (utf8Encode)
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

data PageState = Inited | Updated | Unchanged deriving (Show, Eq, Bounded, Enum)

maxPageStateLength :: Int
maxPageStateLength = maximum $ map (length . show) $ enumFrom (minBound :: PageState)

downloadPage :: String -> IO HTMLPage
downloadPage url = do
    rawPage <- simpleHTTP (getRequest url) >>= getResponseBody
    return $ canonicalizeTags $ parseTags $ utf8Encode rawPage

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
