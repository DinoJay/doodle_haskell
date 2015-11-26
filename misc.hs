import Data.List (transpose, intercalate, intersperse)

-- data Rec t = Rec{title :: String, tsMap :: Map Int (Ts t)}
data Ts t = Ts {st :: t , et:: t , ps :: [String]}  deriving Show
-- test data
test =
    [ Ts "start" "end" ["jan", "nils"]
    , Ts "showTable" "end"   ["babba", "kiran", "nils", "Ralf", "Gisela", "Rudi", "Cairo"]
    ]

-- functions that fill a string (s) to a given width (n) by adding pad
-- character (c) to align left, right, or center
fillLeft n s = s ++ replicate (n - length s) ' '

-- myIntercalate :: Text -> [Text] -> Text
-- myIntercalate t = concat . (intersperse t)
-- intersperse sep (x:xs)  = x : prependToAll sep xs
-- converts a list of items into a table according to a list
-- of column descriptors
-- showTable :: [ColDesc t] -> [t] -> String
showTable ts =
    let header = "Title"
        rows = [[f t | f <- [st, et, (intercalate ", " . map show . ps)]] | t <- ts]
        widths = [maximum $ map length col | col <- transpose $ [header] : rows]
        separator = "+-" ++ ( intercalate "-+-" [replicate width '-' | width <- widths] ) ++ "-+"
        headSep = "+" ++ ( intercalate "--" [replicate width '-' | width <- widths] ) ++ "-----"
        fillCols cols = "| " ++  (intercalate " | " [fillLeft width col | (width, col) <- zip widths cols]++" |\n"++separator)
        fillTitle header =  headSep ++ "\n| " ++ header ++ "\n"++separator
    in
        unlines $ fillTitle header : map fillCols rows

myTest = putStrLn $ showTable test
