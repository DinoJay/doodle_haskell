module Misc (myPrint) where
import Data.List (transpose, intercalate, intersperse)

-- data Rec t = Rec{title :: String, tsMap :: Map Int (Ts t)}
-- test data

-- functions that fill a string (s) to a given width (n) by adding pad
-- character (c) to align left, right, or center
fillLeft n s = s ++ replicate (n - length s) ' '
-- myIntercalate :: Text -> [Text] -> Text
-- myIntercalate t = concat . (intersperse t)
-- intersperse sep (x:xs)  = x : prependToAll sep xs
-- converts a list of items into a table according to a list
-- of column descriptors
-- showTable :: [ColDesc t] -> [t] -> String
showTable header ts =
    let rows = [[f t | f <- [st, et, (intercalate ", " . map show . ps)]] | t <- ts]
        widths = [maximum $ map length col | col <- transpose $ [header] : rows]
        separator = "+-" ++ ( intercalate "-+-" [replicate width '-' | width <- widths] ) ++ "-+"
        headSep = "+" ++ ( intercalate "--" [replicate width '-' | width <- widths] ) ++ "----+"
        fillCols cols = "| " ++  (intercalate " | " [fillLeft width col | (width, col) <- zip widths cols]++" |\n"++separator)
        fillTitle header =  headSep ++ "\n| " ++ header ++ "\n"++separator
    in
        unlines $ fillTitle header : map fillCols rows

myPrint t d = putStrLn $ showTable t d
