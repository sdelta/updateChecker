{-# LANGUAGE TemplateHaskell #-}

module DrawReport(
        SpecificColor,
        alignPrintable,
        alignListOfPrintable,
        printResultColored,
        summaryToPrintable,
        printUncoloredMatrix,
        runTests)        
    where

import Control.Applicative ((<$>), liftA2)
import Data.List (transpose, intersperse)
import System.Console.ANSI(Color(..))
import Test.QuickCheck
import Data.Maybe (fromJust,
                   fromMaybe, 
                   isJust,
                   maybeToList,
                   catMaybes)


import Reporting (PageState(..))
import Printable

instance Show PageState where
    show UpdatedBooksCount = "books count changed"
    show UpdatedBookTitle = "title changed"
    show UpdatedBookLink = "link changed"
    show UpdatedBookSize = "size changed"
    show UpdatedBookDesc = "description changed"
    show Inited = "Inited"
    show Unchanged = "Unchanged"


pageStateToAlignedPrintable :: PageState -> PrintableData
pageStateToAlignedPrintable x = PrintableData (printPageState x) (colorPageState x)
    where
        colorPageState :: PageState -> SpecificColor
        colorPageState Inited = Just Red
        colorPageState Unchanged = Just Yellow
        colorPageState _ = Just Green

        printPageState :: PageState -> String
        printPageState x = show x ++ replicate (maxPageStateLength - length (show x)) ' '

        maxPageStateLength :: Int
        maxPageStateLength = maximum $ map (length . show) $ enumFrom (minBound :: PageState)

alignPrintable :: [Maybe Int] -> Printable -> Printable
alignPrintable alignLens lst = zipWith ($) funcList lst
    where
        funcList :: [PrintableData -> PrintableData]
        funcList = map transform infAlignLens

        infAlignLens = alignLens ++ repeat Nothing

        transform :: Maybe Int -> PrintableData -> PrintableData
        transform Nothing x = x
        transform (Just len) x = x { printData = enlargeIfNeed $ printData x }
            where
                enlargeIfNeed :: String -> String
                enlargeIfNeed str = str ++ replicate (len - length str) ' '

alignListOfPrintable :: [Printable] -> [Printable]
alignListOfPrintable matrix = map (alignPrintable lensLst) matrix
    where
        lensLst :: [Maybe Int]
        lensLst = map (Just . maximum . map (length . printData)) $ transpose matrix

printResultColored :: Printable -> IO ()
printResultColored lst = do
    sequence_ $ map printPrintableData lst
    putStrLn ""

summaryToPrintable :: (String, Int) -> (PageState, String) -> Printable
summaryToPrintable (nick, nickAlignedLen) (pageState, link) = alignedLst
    where
        alignedLst = addSpaces $ alignPrintable [Nothing, Just nickAlignedLen] lst

        lst = [pageStateToAlignedPrintable pageState, uncolored nick, uncolored link]

printUncoloredMatrix :: [[String]] -> IO ()
printUncoloredMatrix matrix = sequence_ $ map printResultColored printableList
    where
        printableList = alignListOfPrintable $ map addSpaces $ map (map uncolored) matrix

addSpaces :: Printable -> Printable
addSpaces lst = uncolored "  " : intersperse (uncolored " ") lst

uncolored :: String -> PrintableData
uncolored str = PrintableData str Nothing


--tests
prop_pageStateToAlignedPrintable :: PageState -> PageState -> Bool
prop_pageStateToAlignedPrintable s1 s2 = f s1 == f s2
    where
        f = length . printData . pageStateToAlignedPrintable

prop_alignPrintable :: [String] -> [Maybe Int] -> Bool
prop_alignPrintable strLst lenLst = all id $ catMaybes $ zipWith (liftA2 (<=)) lenLst $ map Just resLenLst
    where
        resLenLst :: [Int]
        resLenLst = map (length . printData) $ alignPrintable lenLst $ map uncolored strLst

prop_alignListOfPrintable :: Int -> Int -> [String] -> Property
prop_alignListOfPrintable x y lst = (x /= 0 && y /= 0 && lst /= []) ==> result
    where
        result = all isAllEqual $ transpose appliedTest

        isAllEqual :: Eq a => [a] -> Bool
        isAllEqual (x:xs) = all (x==) xs

        appliedTest = map (map (length . printData)) $ alignListOfPrintable test

        test = take x $ lstToMatrix y $ cycle $ map uncolored lst

        lstToMatrix :: Int -> [a] -> [[a]]
        lstToMatrix 0 _ = error "using lstToMatrix with 0 lst size"
        lstToMatrix _ [] = error "using lstToMatrix with not infinite list"
        lstToMatrix x lst = take x lst : (lstToMatrix x $ drop x lst)

runTests = $quickCheckAll 
