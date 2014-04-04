module PrintMatrix(
        SpecificColor,
        alignFields,
        alignStrings,
        alignStrings',
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

import HTML (PageState(..))

type SpecificColor = Maybe Color

alignFields :: [Int] -> [String] -> [String]
alignFields lengths strList = zipWith enlargeStr strList lengths
    where
        enlargeStr str len = str ++ replicate (len - length str) ' '

alignStrings :: [Maybe Int] -> [[String]] -> [[String]]
alignStrings precalcLengths matrix = 
    let
        lengthsMatrix = map (map length) matrix
        lengthsList = map maximum $ transpose lengthsMatrix
        quickLengthsList = zipWith fromMaybe lengthsList precalcLengths
        getNSpaces n = replicate n ' '
    in
        map (alignFields quickLengthsList) matrix

joinLists :: Int -> [String] -> [String] -> [String]
joinLists gapWidth list1 list2 = list1 ++ [gap] ++ list2
    where
        gap = replicate gapWidth ' '

alignStrings' :: [[String]] -> [[String]]
alignStrings' = alignStrings $ repeat Nothing

isEqualPrefix prefix str = (take len str == prefix) && (drop len str == replicate (length str - len) ' ')
    where 
        len = length prefix

colorPageState :: String -> SpecificColor
colorPageState str | isEqualPrefix (show Inited) str    = Just Red
                   | isEqualPrefix (show Unchanged) str = Just Yellow
                   | isEqualPrefix (show Updated) str   = Just Green
                   | otherwise = Nothing

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
