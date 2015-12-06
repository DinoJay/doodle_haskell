import Doodle

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import System.Locale
import Data.Time

-- Test input Start --
-- singleTsMap = Map.singleton 0 ( Ts (Dt "9-8-2011 10:54 AM") (Dt "9-8-2012 10:54 AM") ["jan", "nils"] )
-- testTs0 = Map.insert 1 ( Ts (Dt "10-8-2013 10:54 AM" ) (Dt "10-8-2014 10:54 AM" ) ["babba", "kiran", "nils", "Ralf", "Gisela", "Rudi", "Cairo"] ) singleTsMap
--
-- testTs1 = Map.insert 2 ( Ts (Dt "9-8-2012 10:54 AM" ) (Dt "9-8-2013 10:54 AM" ) ["babba", "kiran", "nils", "Ralf", "Gisela", "Rudi", "Cairo"] ) testTs0
--
-- dateString0 = Just(Right(Dt "9-8-2015 10:54 AM"))
-- dateString1 = Just(Right(Dt "9-8-2016 10:54 AM"))
--
-- dateInputHigher  = Just(Right(Dt "9-8-2015 10:54 AM +0400", Dt "9-8-2016 10:54 AM +0400"))
-- dateInputMiddle0 = Just(Right(Dt "9-8-2005 9:54 AM +0400", Dt "9-8-2007 10:30 AM +0400"))
-- dateInputMiddle1 = Just(Right(Dt "9-8-2003 9:54 AM +0400", Dt "9-8-2004 10:30 AM +0400"))
-- dateInputLow     = Just(Right(Dt "9-8-2000 9:20 AM +0400", Dt "9-8-2001 10:30 AM +0400"))
-- dateSmallDiff    = Just(Right(Dt "3-8-2016 10:54 AM +0400", Dt "4-8-2016 9:54 AM +0400"))
--
-- testRec = Rec{title="xmas", tsMap=testTs1}
--
-- testList =
--     [ Ts "start" "end" ["jan", "nils"]
--     , Ts "showTable" "end"   ["babba", "kiran", "nils", "Ralf", "Gisela", "Rudi", "Cairo"]
--     , Ts "showTable" "end"   []
--     ]
-- Test input End

-- Set timeslot like this in the execution of the Doodle
-- Just(Right(Dt "9-8-2015 10:54 AM +0400", Dt "9-8-2016 10:54 AM +0400"))


-- Doodle Record
data Rec t = Rec{title :: String, tsMap :: Map Int (Ts t)}

instance (Ord t, Show t ) => Show (Rec t) where
  show = myPrint

-- Timeslot type
data Ts t = Ts {st :: t , et:: t , ps :: [String]} deriving Show

-- introduce ordering of timeslot by start time assuming type t is ordered
instance Ord t => Ord (Ts t) where
    (<=) (a) (b) = st a <= st b

instance Ord t => Eq (Ts t) where
    (==) (a) (b) = st a == st b


-- Date type
data Dt = Dt String deriving (Read)

-- helper method for time conversion from string
timeFromString:: String -> UTCTime
timeFromString ds = readTime defaultTimeLocale "%-d-%-m-%Y %l:%M %p %z" ( ds :: String ) :: UTCTime

-- simple show unwrapper
instance Show Dt where
  show (Dt t)= show t

-- make Dt comparable
instance Eq (Dt) where
    (==) (Dt a) (Dt b) = timeFromString a == timeFromString b

-- give it an order
instance Ord (Dt) where
    (<=) (Dt a) (Dt b) = timeFromString a <= timeFromString b


-- implement Doodle
instance Doodle Rec where
    initialize = myinit
    add = myAddTs
    remove = myRemove
    toogle = myToogle


myinit :: String -> Rec t
myinit s = Rec{title = s, tsMap = Map.empty}

-- add timeslot into Rec
myAddTs:: Ord t => (t,t) -> Rec t -> Rec t
myAddTs ts d = Rec{title = title d, tsMap = myInsert ts (tsMap d)}

-- add timeslot into timeslot map with checking for timeclashes
myInsert :: (Enum k, Num k, Ord t, Ord k) => (t,t) -> Map k (Ts t) -> Map k (Ts t)
myInsert (t2, t3) tss =
    if noTimeClash tss (t2, t3) then orderMap $ Map.insert (myFreshKey tss) Ts{st = t2, et = t3, ps =[]} tss else tss

-- reorder Map with indexes after a new timeslot has been inserted
orderMap:: (Enum k, Num k, Ord a1, Ord k) => Map a a1 -> Map k a1
orderMap xs = Map.fromList $ zip [1..] list
    where list = List.sort $ List.map snd ( Map.toList xs )

-- remove timeslot with given index
myRemove :: Int -> Rec t -> Rec t
myRemove k d = Rec{title = title d, tsMap = Map.delete k (tsMap d)}

myToogle :: String -> Int -> Rec t -> Rec t
myToogle v k d = Rec{title = title d, tsMap = addPs k v (tsMap d) }

addPs :: Ord k => k -> String -> Map k (Ts t) -> Map k (Ts t)
addPs k v = Map.update( \x -> Just Ts{st = st x, et = et x, ps = if elem v (ps x) then ps x else v : ps x}) k

-- get a fresh key from timeslot map
myFreshKey :: (Enum a, Num a) => Map a b -> a
myFreshKey p
    | Map.null p == False = succ (fst $ Map.findMax p)
    | otherwise = 1

-- check if there are time clashes in the timeslot map for a given time
-- range
noTimeClash :: Ord a => Map k (Ts a) -> (a, a) -> Bool
noTimeClash xs (t2, t3) = List.null $ Map.elems $ Map.filter (\y -> ( st y >= t2 && st y <= t3 || (et y >= t2 && et y <= t3) )) xs

-- helper for above
data RecMap k v = RecMap (Map k v) k deriving (Show)

-- implement Doodle with RecMap
instance Pool RecMap  where
        freshKey (RecMap _ i) = succ i
        get k (RecMap p _) = Map.lookup k p
        set k d (RecMap p i) = mySet k d (RecMap p i)

mySet :: Ord k => k -> v -> RecMap k v -> RecMap k v
mySet k v (RecMap d _) = RecMap (Map.insert k v d) k

main :: IO ()
main = do
          let w = RecMap Map.empty 0 :: RecMap Int (Rec Dt)
          run w

-- show table function inspired by :
-- http://stackoverflow.com/questions/5929377/format-list-output-in-haskell

--  fill up string with whitespace with n chars
fillLeft :: Int -> String -> String
fillLeft n s = s ++ replicate (n - length s) ' '

showTable:: Show b => String -> [Ts b] -> String
showTable tableTitle ts =
    let rows = [[f t | f <- [show.st, show.et, List.intercalate ", " . map show . ps]] | t <- ts]
        widths = [maximum $ map length col | col <- List.transpose $ [tableTitle] : rows]
        separator = if length ts == 0 then "+---" ++ innerSep ++ "-+"
                                      else "+-" ++ innerSep ++ "-+"
            where innerSep = (List.intercalate "-+-" [replicate width '-' | width <- widths] )
        headSep = "+" ++ (List.intercalate "--" [replicate width '-' | width <- widths] ) ++ "----+"
        fillCols cols = "| " ++  (List.intercalate " | " [fillLeft width col | (width, col) <- zip widths cols]++" |\n"++separator)
        fillTitle t =  headSep ++ "\n| " ++ t ++ "\n"++separator
    in
        unlines $ fillTitle tableTitle : map fillCols rows

myPrint :: Show b => Rec b -> String
myPrint r = showTable (title r) (List.map snd $ Map.toList (tsMap r))

