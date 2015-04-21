module Printable (
        SpecificColor,
        PrintableData(..),
        Printable,
        toPrintableData,
        setColor,
        toPrintableDataWithColor,
        printPrintableData
    )
    where

import Control.Applicative ((<$>))
import Data.Maybe (maybeToList)
import System.Console.ANSI

type SpecificColor = Maybe Color

data PrintableData = PrintableData {
    printData :: String,
    colorFunc :: SpecificColor
}

type Printable = [PrintableData]

toPrintableData :: Show a => a -> PrintableData
toPrintableData x = PrintableData (show x) Nothing 

setColor :: SpecificColor -> PrintableData -> PrintableData
setColor color (PrintableData str _) = PrintableData str color

toPrintableDataWithColor :: Show a => a -> (a -> SpecificColor) -> PrintableData
toPrintableDataWithColor x f = setColor (f x) $ toPrintableData x 

printPrintableData :: PrintableData -> IO ()
printPrintableData (PrintableData str color) = do
    setSGR $ maybeToList (SetColor Foreground Dull <$> color)
    putStr str
    setSGR []
