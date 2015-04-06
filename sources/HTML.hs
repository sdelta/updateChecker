module HTML(
        downloadPage, 
        getBooksDesc, 
        pageToPageTitleHash, 
        compareBookDesc,
        printPageState,
        compareBookLst,
        getBookLinkByIndex,

        BookDescription, 
        PageState(..),
        maxPageStateLength)
    where

import Control.Monad.State
import Control.Applicative ((<$>))
import Data.Hashable (hash)
import Data.Maybe (isJust, fromJust)
import Text.HTML.TagSoup as HTML
import Network.HTTP (simpleHTTP, 
                     getRequest, 
                     getResponseBody)

type HTMLPage = [Tag String]

data BookDescription = BookDescription {
    bookTitle :: String,
    bookLink :: String,
    bookSize  :: Int,
    bookDescription :: String
} deriving (Read, Show, Eq)

-- in order of precendence of printing (firsts suppress following)
data PageState = UpdatedBooksCount | 
                UpdatedBookTitle | 
                UpdatedBookLink |
                UpdatedBookSize |
                UpdatedBookDesc |
                Inited | 
                Unchanged deriving (Show, Eq, Ord, Bounded, Enum)

getBookLinkByIndex :: [BookDescription] -> String -> Maybe Int -> String
getBookLinkByIndex books _ index | isJust index = bookLink (books !! fromJust index)
getBookLinkByIndex _ defValue _ = defValue

printPageState :: PageState -> String
printPageState UpdatedBookTitle = "title changed"
printPageState UpdatedBookLink = "link changed"
printPageState UpdatedBookSize = "size changed"
printPageState UpdatedBookDesc = "description changed"
printPageState UpdatedBooksCount = "books count changed"
printPageState x = show x


compareBookDesc :: BookDescription -> BookDescription -> PageState
compareBookDesc a b | a == b = Unchanged
compareBookDesc a b | bookTitle a /= bookTitle b = UpdatedBookTitle
compareBookDesc a b | bookLink a /= bookLink b = UpdatedBookLink
compareBookDesc a b | bookSize a /= bookSize b = UpdatedBookSize
compareBookDesc a b = UpdatedBookDesc

compareBookLst :: [BookDescription] -> [BookDescription] -> (PageState, Maybe Int)
compareBookLst a b | length a /= length b = (UpdatedBooksCount, Nothing)
compareBookLst a b = discardIndexIfUnchanged $ minimum $ addNumeration $ zipWith compareBookDesc a b
    where
        addNumeration lst = zip lst $ map Just [0..]

        discardIndexIfUnchanged (a, b) | a == Unchanged = (a, Nothing)
        discardIndexIfUnchanged (a, b) = (a, b)

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
            modify $ dropWhile (~/= "<a>")
            link <- fromAttrib "href" <$> head <$> get
            modify tail
            title <- extractText <$> takeWhile (~/= " &nbsp; ") <$> get
            modify (tail . tail . dropWhile  (~/= " &nbsp; "))
            size <- (read <$> init <$> fromTagText <$> head <$> get) :: State HTMLPage Int
            modify $ dropWhile (~/= "<dd>")
            modify $ takeWhile (~/= "</dl>")
            description <- extractText <$> get
            return $ BookDescription title link size description

getBooksDesc :: HTMLPage -> [BookDescription]
getBooksDesc page = map pageToBookDescription $ dividePageToBookSubPages page

-- assume that calling pageTagsToPageTitleHash be after canonicalizeTags function
pageToPageTitleHash :: HTMLPage -> Int
pageToPageTitleHash page = flip evalState page $ do
    modify (tail . dropWhile (~/= "<h3>"))
    modify $ takeWhile (~/= "</h3>")
    pageTitle <- concatMap fromTagText <$> filter isTagText <$> get
    return $ abs $ hash pageTitle
