module HTML(
        Book(..), 
        AuthorPage(..),
        emptyPage,
        downloadAuthorPage)
    where

import Control.Monad.State
import Control.Applicative ((<$>), (<*>))
import Data.Hashable (hash)
import Data.Maybe (isJust, fromJust)
import Data.List (sort)
import Text.HTML.TagSoup as TG
import Network.HTTP (simpleHTTP, 
                     getRequest, 
                     getResponseBody)

data AuthorPage = AuthorPage {
    books :: [Book],
    pageHash :: Int,
    url :: String
} deriving (Read, Show)

emptyPage :: AuthorPage
emptyPage = AuthorPage [] 0 "empty author page url"

data Book = Book {
    bookTitle :: String,
    bookLink :: String,
    bookSize  :: Int,
    bookDescription :: [String]
} deriving (Read, Show, Eq, Ord)

type HTMLPage = [Tag String]

downloadAuthorPage :: String -> IO AuthorPage
downloadAuthorPage url = AuthorPage <$> descriptions <*> hash <*> return url
    where
        page = downloadPage url
        descriptions = sort <$> map subpageToBook <$> dividePageToBookSubPages <$> page
        hash = pageToPageTitleHash <$> page

pageToPageTitleHash :: HTMLPage -> Int
pageToPageTitleHash page = flip evalState page $ do
    modify (tail . dropWhile (~/= "<h3>"))
    modify $ takeWhile (~/= "</h3>")
    pageTitle <- concatMap fromTagText <$> filter isTagText <$> get
    return $ abs $ hash pageTitle

dividePageToBookSubPages :: HTMLPage -> [HTMLPage]
dividePageToBookSubPages page = filter isBeginingOfDesc $ TG.sections (~== TagOpen "dl" []) page
    where
        beginingOfDesc :: HTMLPage
        beginingOfDesc = [TagOpen "dl" [], TagOpen "dt" [], TagOpen "li" []]

        isBeginingOfDesc :: HTMLPage -> Bool
        isBeginingOfDesc list = take (length beginingOfDesc) list == beginingOfDesc

subpageToBook :: HTMLPage -> Book
subpageToBook tagList = 
    let
        extractText :: HTMLPage -> String
        extractText page = concatMap fromTagText $ filter isTagText page
    in
        flip evalState tagList $ do
            modify $ dropWhile (~/= "<a>")
            link <- fromAttrib "href" <$> head <$> get
            modify tail
            title <- extractText <$> takeWhile (~/= " &nbsp; ") <$> get
            modify (tail . tail . dropWhile  (~/= " &nbsp; "))
            size <- (read <$> init <$> fromTagText <$> head <$> get) :: State HTMLPage Int
            modify $ dropWhile (~/= "<dd>")
            modify $ takeWhile (~/= "</dl>")
            description <- lines <$> extractText <$> get
            return $ Book title link size description

downloadPage :: String -> IO HTMLPage
downloadPage url = do
    rawPage <- simpleHTTP (getRequest url) >>= getResponseBody
    return $ canonicalizeTags $ parseTags rawPage
