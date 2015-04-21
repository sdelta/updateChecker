module Reporting (ComparisionReport(..),
                BookDiff(..),
                PageState(..),
                reportDifferencies,
                isBooksTheSameProbability,
                summarize)
    where

import Data.Maybe (catMaybes)
import Data.Function (on)

import HTML(Book(..), 
        AuthorPage(..))

import Algo (Recipe,
            Edit(..),
            levenshteinDist,
            getTransformationRecipe,
            recipeToNew)

import Test.QuickCheck (Arbitrary(..),
                        arbitraryBoundedEnum)

import Debug.Trace

data ComparisionReport = Report {
    baseLink :: String,
    booksAdded :: [Book],
    booksRemoved :: [Book],
    booksChanged :: [BookDiff]
}

data BookDiff = BookDiff {
    titleDiff :: Recipe Char,
    linkDiff :: Recipe Char,
    sizeDiff :: Int,
    descriptionDiff :: [(Edit String, [Edit Char])]
}

-- in order of precendence of printing (firsts suppress following)
data PageState = UpdatedBooksCount | 
                UpdatedBookTitle | 
                UpdatedBookLink |
                UpdatedBookSize |
                UpdatedBookDesc |
                Inited | 
                Unchanged deriving (Eq, Ord, Bounded, Enum)

instance Arbitrary PageState where
    arbitrary = arbitraryBoundedEnum

summarize :: ComparisionReport -> (PageState, String)
summarize (Report link added [] []) = (Inited, link)
summarize (Report link added removed _) | not (null added) || not (null removed) = (UpdatedBooksCount, link)
summarize (Report link _ _ changed) = maximum $ map bookDiffToResult changed
    where
        bookDiffToResult :: BookDiff -> (PageState, String)
        bookDiffToResult diff | any isNotRemain $ titleDiff diff = (UpdatedBookTitle, getCurLink diff)
        bookDiffToResult diff | any isNotRemain $ linkDiff diff = (UpdatedBookLink, getCurLink diff)
        bookDiffToResult diff | sizeDiff diff /= 0 = (UpdatedBookSize, getCurLink diff)
        bookDiffToResult diff | any (isNotRemain . fst) $ descriptionDiff diff = (UpdatedBookDesc, getCurLink diff)
        bookDiffToResult _ = (Unchanged, link)

        getCurLink :: BookDiff -> String
        getCurLink (BookDiff _ linkRecipe _ _) = recipeToNew linkRecipe

        isNotRemain :: Edit a -> Bool
        isNotRemain (Remain _) = False
        isNotRemain _ = True

-- isBooksTheSameProbability evaluate probability of event that book1 and book2 are the same book with
-- several differencies
isBooksTheSameProbability :: Book -> Book -> Double
isBooksTheSameProbability book1 book2 = titlePoints + linkPoints + sizePoints + descPoints
    where
        linkPoints = 0.25 * if on (==) bookLink book1 book2 then 1 else 0
        sizePoints = 0.25 * if abs (bookSize book1 - bookSize book2) < 40 then 1 else 0
        descPoints = 0.15 * (inversePercentFromMax (concat . bookDescription) $ descDistance)

        titlePoints = 0.35 * (inversePercentFromMax bookTitle $ on levenshteinDist bookTitle book1 book2)

        descDistance = on levenshteinDist (concat . bookDescription) book1 book2

        inversePercentFromMax :: (Book -> String) -> Int -> Double
        inversePercentFromMax f x = 1 - result
            where
                result = if divisor == 0 then 0 else intToDouble x / intToDouble divisor

                divisor = on max (length . f) book1 book2

                intToDouble :: Int -> Double
                intToDouble = fromInteger . toInteger

reportDifferencies :: AuthorPage -> AuthorPage -> ComparisionReport
reportDifferencies (AuthorPage oldBooks _ _) (AuthorPage newBooks _ url) = report
    where
        report = Report url added removed $ map (uncurry reportBookDifferencies) coupledBooks

        added = filter (isNotInTheList $ map fst coupledBooks) newBooks
        removed = filter (isNotInTheList $ map snd coupledBooks) oldBooks

        isNotInTheList lst x = not (x `elem` lst)

        -- coupledBooks may contain sublist [(b1, b2), (b3, b2)]
        -- if it is inappropriate than coupledBooks should be rewrited
        coupledBooks :: [(Book, Book)]
        coupledBooks = catMaybes $ map (findCoupleIndex $ oldBooks) $ newBooks

        -- if isBooksTheSameProbability b1 b2 < probOfImpossible then b1 and b2 are different books
        probOfImpossible :: Double
        probOfImpossible = 0.35

        findCoupleIndex :: [Book] -> Book -> Maybe (Book, Book)
        findCoupleIndex [] _ = Nothing
        findCoupleIndex lst b = 
            let
                (prob, index) = maximum $ flip zip [0..] $ map (isBooksTheSameProbability b) lst
            in if prob < probOfImpossible
                then Nothing
                else Just (b, lst !! index)

reportBookDifferencies :: Book -> Book -> BookDiff
reportBookDifferencies old new = BookDiff title link size desc
    where
        title = on getTransformationRecipe bookTitle old new
        link = on getTransformationRecipe bookLink old new
        size = on (-) bookSize old new
        desc = zip descRecipe $ catMaybes $ map getRecipeOfReplaced descRecipe
            where
                getRecipeOfReplaced :: Edit String -> Maybe (Recipe Char)
                getRecipeOfReplaced (Replace old new) = Just $ getTransformationRecipe old new
                getRecipeOfReplaced _ = Nothing

        descRecipe = on getTransformationRecipe bookDescription old new
