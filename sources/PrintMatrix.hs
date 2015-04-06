module PrintMatrix(
        SpecificColor,
        alignFields,
        alignStrings,
        joinLists,
        colorPageState,
        colorDefault,
        printResultColored)        
    where

import Data.List (transpose)
import System.Console.ANSI
import Data.Maybe (fromJust,
                   fromMaybe, 
                   isJust)

import HTML (PageState(..), printPageState)

type SpecificColor = Maybe Color

alignFields :: [Int] -> [String] -> [String]
alignFields lengths strList = zipWith enlargeStr strList lengths
    where
        enlargeStr str len = str ++ replicate (len - length str) ' '

alignStrings :: [[String]] -> [[String]]
alignStrings matrix = map (alignFields fieldsLength) matrix
    where
        fieldsLength = map (maximum . map length) $ transpose matrix

joinLists :: Int -> [String] -> [String] -> [String]
joinLists gapWidth list1 list2 = list1 ++ [gap] ++ list2
    where
        gap = replicate gapWidth ' '

isEqualPrefix prefix str = (take len str == prefix) && (drop len str == replicate (length str - len) ' ')
    where 
        len = length prefix

colorPageState :: String -> SpecificColor
colorPageState str | isEqualPrefix (printPageState Inited) str    = Just Red
                   | isEqualPrefix (printPageState Unchanged) str = Just Yellow
                   | otherwise = Just Green

colorDefault :: String -> SpecificColor
colorDefault _ = Nothing

printResultColored :: [String -> SpecificColor] -> Int -> [String] -> IO ()
printResultColored funcList shift strList = 
    let
        printInColor :: (String -> SpecificColor) -> String -> IO ()
        printInColor func str = do
            let specColor = func str
            setSGR [SetColor Foreground Dull (fromJust specColor) | isJust specColor]
            putStr str
            setSGR []
    in do
        putStr $ replicate shift ' '
        mapM_ (uncurry printInColor) $ zip funcList strList
        putStrLn ""
