import Doodle

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Text.PrettyPrint
import Data.List as List
import Data.List (transpose, intercalate, intersperse)

import System.Locale
import Data.Time
import Data.Time.Format


singleTsMap = Map.singleton 0 ( Ts (Dt "9-8-2011 10:54 AM") (Dt "9-8-2012 10:54 AM") ["jan", "nils"] )

testTs0 = Map.insert 1 ( Ts (Dt "10-8-2013 10:54 AM" ) (Dt "10-8-2014 10:54 AM" ) ["babba", "kiran", "nils", "Ralf", "Gisela", "Rudi", "Cairo"] ) singleTsMap

testTs1 = Map.insert 2 ( Ts (Dt "9-8-2012 10:54 AM" ) (Dt "9-8-2013 10:54 AM" ) ["babba", "kiran", "nils", "Ralf", "Gisela", "Rudi", "Cairo"] ) testTs0

dateString0 = Just(Right(Dt "9-8-2015 10:54 AM"))
dateString1 = Just(Right(Dt "9-8-2016 10:54 AM"))

dateInputHigher = Just(Right(Dt "9-8-2015 10:54 AM", Dt "9-8-2016 10:54 AM"))
dateInputMiddle0 = Just(Right(Dt "9-8-2005 9:54 AM", Dt "9-8-2007 10:30 AM"))
dateInputMiddle1 = Just(Right(Dt "9-8-2003 9:54 AM", Dt "9-8-2004 10:30 AM"))
dateInputLow = Just(Right(Dt "9-8-2000 9:54 AM", Dt "9-8-2001 10:30 AM"))

testRec = Rec{title="xmas", tsMap=testTs1}

testList =
    [ Ts "start" "end" ["jan", "nils"]
    , Ts "showTable" "end"   ["babba", "kiran", "nils", "Ralf", "Gisela", "Rudi", "Cairo"]
    , Ts "showTable" "end"   []
    ]

data Ts t = Ts {st :: t , et:: t , ps :: [String]} deriving Show
data Rec t = Rec{title :: String, tsMap :: Map Int (Ts t)}

data Dt = Dt String deriving (Read)

instance Show Dt where
  show (Dt t)= show t

instance Eq (Dt) where
    (==) (Dt a) (Dt b) = (timeFromString a) == (timeFromString b)

instance Ord (Dt) where
    (<=) (Dt a) (Dt b) = (timeFromString a) <= (timeFromString b)


instance Ord t => Eq (Ts t) where
    (==) (a) (b) = (st a) == (st b)
instance Ord t => Ord (Ts t) where
    (<=) (a) (b) = (st a) <= (st b)

instance Doodle Rec where
    initialize x = myinit x
    add (t0, t1) d = myAddTsWrapper (t0, t1) d
    remove k d = myRemove k d
    toogle s k d = myToogle s k d

instance (Ord t, Show t ) => Show (Rec t) where
  show r = myPrint r

myinit s = Rec{title = s, tsMap = Map.empty}
myAddTsWrapper ts d = Rec{title = title d, tsMap = myInsert ts (tsMap d)}

-- myInsertTs (t0, t1) tss = (Map.insert (myFreshKey tss) Ts{st = t0, et = t1, ps =[] } tss)

myInsert (t2, t3) tss =
    if (getTimeClash tss (t2, t3)) then (orderMap $ Map.insert (myFreshKey tss) Ts{st = t2, et = t3, ps =[]} tss) else tss

orderMap xs = Map.fromList $ ( zip [1..] list )
    where list = List.sort $ ( List.map snd ( Map.toList xs ) )

myRemove k d = Rec{title = title d, tsMap = Map.delete k (tsMap d)}

myToogle v k d = Rec{title = title d, tsMap = addPs k v (tsMap d) }

addPs k v tsm = Map.update( \x -> Just (Ts{st = st x, et = et x, ps = v : (ps x)})) k tsm

myFreshKey p
    | Map.null p == False = succ (fst $ Map.findMax p)
    | otherwise = 1

getTimeClash xs (t2, t3) = List.null $ Map.elems $ Map.filter (\y -> ( st y >= t2 && st y <= t3 || (et y >= t2 && et y <= t3) )) xs

-- helper for above
data RecMap k v = RecMap (Map k v) k deriving (Show)

instance Pool RecMap  where
        freshKey (RecMap _ i) = succ i
        get k (RecMap p _) = Map.lookup k p
        set k d (RecMap p i) = mySet k d (RecMap p i)

mySet k v (RecMap d _) = RecMap (Map.insert k v d) k

main :: IO ()
main = do
          let w = RecMap Map.empty 0 :: RecMap Int (Rec Dt)
          run w


fillLeft n s = s ++ replicate (n - length s) ' '
-- myIntercalate :: Text -> [Text] -> Text
-- myIntercalate t = concat . (intersperse t)
-- intersperse sep (x:xs)  = x : prependToAll sep xs
-- converts a list of items into a table according to a list
-- of column descriptors
-- showTable :: [ColDesc t] -> [t] -> String
showTable header ts =
    let rows = [[f t | f <- [show.st, show.et, (intercalate ", " . map show . ps)]] | t <- ts]
        widths = [maximum $ map length col | col <- transpose $ [header] : rows]
        separator = "+-" ++ ( intercalate "-+-" [replicate width '-' | width <- widths] ) ++ "-+"
        headSep = "+" ++ ( intercalate "--" [replicate width '-' | width <- widths] ) ++ "----+"
        fillCols cols = "| " ++  (intercalate " | " [fillLeft width col | (width, col) <- zip widths cols]++" |\n"++separator)
        fillTitle header =  headSep ++ "\n| " ++ header ++ "\n"++separator
    in
        unlines $ fillTitle header : map fillCols rows

myPrint r = showTable (title r) (List.map snd $ Map.toList (tsMap r))

timeFromString ds = (readTime defaultTimeLocale "%-d-%-m-%Y %l:%M %p" ( ds :: String ) ) :: UTCTime
